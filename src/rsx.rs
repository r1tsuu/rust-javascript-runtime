use std::{
    backtrace::Backtrace,
    cell::RefCell,
    collections::HashMap,
    ptr,
    rc::Rc,
    sync::{LazyLock, Mutex, OnceLock},
};

use crate::parser::{BinaryOperator, Expression, Program, Statement, UnaryOperator};

#[derive(Debug)]
pub struct RsxError {
    backtrace: Backtrace,
    message: String,
}

impl RsxError {
    pub fn new<T>(message: T, backtrace: Backtrace) -> Self
    where
        T: ToString,
    {
        RsxError {
            message: message.to_string(),
            backtrace,
        }
    }
}

macro_rules! rsx_err {
    ($($arg:tt)*) => {
        RsxError::new(format!("ERROR: {}", format!($($arg)*)), Backtrace::capture())
    };
}

pub struct Heap {
    address: u64,
    memory: HashMap<u64, *mut Value>,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            address: 0,
            memory: HashMap::new(),
        }
    }

    pub fn alloc(&mut self, value: Value) -> HeapRef {
        let address = self.address;
        self.memory.insert(address, Box::into_raw(Box::new(value)));
        self.address += 1;

        return HeapRef::new(address);
    }

    pub fn alloc_number(&mut self, value: f64) -> HeapRef {
        self.alloc(Value::Number(value))
    }

    pub fn alloc_string(&mut self, value: String) -> HeapRef {
        self.alloc(Value::String(value))
    }

    pub fn alloc_object(&mut self) -> HeapRef {
        self.alloc(Value::Object(Object::new()))
    }

    pub fn get_undefined(&mut self) -> HeapRef {
        UNDEFINED_CELL
            .get_or_init(|| self.alloc(Value::Undefined))
            .clone()
    }

    pub fn get_null(&mut self) -> HeapRef {
        NULL_CELL.get_or_init(|| self.alloc(Value::Null)).clone()
    }

    pub fn get_false(&mut self) -> HeapRef {
        FALSE_CELL
            .get_or_init(|| self.alloc(Value::Boolean(false)))
            .clone()
    }

    pub fn get_true(&mut self) -> HeapRef {
        TRUE_CELL
            .get_or_init(|| self.alloc(Value::Boolean(true)))
            .clone()
    }

    pub fn get_boolean(&mut self, value: bool) -> HeapRef {
        if value {
            self.get_true()
        } else {
            self.get_false()
        }
    }
}

static UNDEFINED_CELL: OnceLock<HeapRef> = OnceLock::new();
static NULL_CELL: OnceLock<HeapRef> = OnceLock::new();
static FALSE_CELL: OnceLock<HeapRef> = OnceLock::new();
static TRUE_CELL: OnceLock<HeapRef> = OnceLock::new();

#[derive(Debug)]
pub enum Value {
    String(String),
    Number(f64),
    Object(Object),
    Boolean(bool),
    Undefined,
    Null,
}

impl Value {
    pub fn try_number(&self) -> Result<f64, RsxError> {
        match self {
            Value::Number(value) => Ok(*value),
            _ => Err(rsx_err!("Failed to parse {self:#?} to number")),
        }
    }

    pub fn try_boolean(&self) -> Result<bool, RsxError> {
        match self {
            Value::Boolean(value) => Ok(*value),
            _ => Err(rsx_err!("Failed to parse {self:#?} to boolean")),
        }
    }

    pub fn try_string(&self) -> Result<String, RsxError> {
        match self {
            Value::String(value) => Ok(value.clone()),
            _ => Err(rsx_err!("Failed to parse {self:#?} to number")),
        }
    }

    pub fn try_object(&mut self) -> Result<&mut Object, RsxError> {
        match self {
            Value::Object(object) => Ok(object),
            _ => Err(rsx_err!("Failed to parse {self:#?} to object")),
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    pub fn is_undefined(&self) -> bool {
        matches!(self, Value::Undefined)
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Undefined => false,
            Value::Object(_) => true,
            Value::Boolean(value) => *value,
            Value::String(value) => value.len() > 0,
            Value::Number(value) => *value != 0.0 && !value.is_nan(),
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Value::String(value) => value.clone(),
            Value::Number(value) => value.to_string(),
            Value::Boolean(value) => value.to_string(),
            Value::Object(_) => "[object Object]".to_string(),
            Value::Null => "null".to_string(),
            Value::Undefined => "undefined".to_string(),
        }
    }

    fn equal_internal(&self, other: &Value, is_strict: bool) -> bool {
        match self {
            Value::Null => {
                if other.is_null() {
                    true
                } else if !is_strict {
                    if other.is_undefined() {
                        true
                    } else {
                        self.as_string() == other.as_string()
                    }
                } else {
                    false
                }
            }
            Value::Undefined => {
                if other.is_undefined() {
                    true
                } else if !is_strict {
                    if other.is_null() {
                        true
                    } else {
                        self.as_string() == other.as_string()
                    }
                } else {
                    false
                }
            }
            Value::Object(value) => {
                if let Value::Object(other) = other {
                    ptr::eq(value, other)
                } else if let Value::String(other) = other {
                    if !is_strict {
                        self.as_string() == *other
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            Value::Boolean(value) => {
                if let Value::Boolean(other) = other {
                    value == other
                } else if !is_strict {
                    match other {
                        Value::String(other) => *value == (other.len() > 0),
                        Value::Number(other) => *value == (*other != 0.0),
                        _ => false,
                    }
                } else {
                    false
                }
            }
            Value::String(value) => {
                if let Value::String(other) = other {
                    other == value
                } else if !is_strict {
                    match other {
                        Value::Boolean(other) => (value.len() > 0) == *other,
                        Value::Number(other) => {
                            if (value.len() == 0 && *other == 0.0) || (other.to_string() == *value)
                            {
                                true
                            } else {
                                false
                            }
                        }
                        _ => false,
                    }
                } else {
                    false
                }
            }
            Value::Number(value) => {
                if let Value::Number(other) = other {
                    *other == *value
                } else if !is_strict {
                    match other {
                        Value::Boolean(other) => (*value != 0.0) == *other,
                        Value::String(other) => {
                            if (other.len() == 0 && *value == 0.0) || (value.to_string() == *other)
                            {
                                true
                            } else {
                                false
                            }
                        }
                        _ => false,
                    }
                } else {
                    false
                }
            }
        }
    }

    pub fn add(&self, other: &Value) -> Value {
        match self {
            Value::String(value) => {
                if let Value::String(other) = other {
                    Value::String(format!("{}{}", value, other))
                } else {
                    Value::String(format!("{}{}", value, other.as_string()))
                }
            }
            Value::Number(value) => match other {
                Value::String(other) => Value::String(format!("{}{}", value, other)),
                Value::Number(other) => Value::Number(value + other),
                Value::Null => Value::Number(*value),
                Value::Undefined => Value::Number(f64::NAN),
                Value::Object(_) => Value::String(format!("{}{}", value, other.as_string())),
                Value::Boolean(other) => Value::Number(*value + if *other { 1.0 } else { 0.0 }),
            },
            _ => unimplemented!(),
        }
    }

    pub fn sub(&self, other: &Value) -> Value {
        match self {
            Value::Number(value) => match other {
                Value::Number(other) => Value::Number(value - other),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn multiply(&self, other: &Value) -> Value {
        match self {
            Value::Number(value) => match other {
                Value::Number(other) => Value::Number(value * other),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn div(&self, other: &Value) -> Value {
        match self {
            Value::Number(value) => match other {
                Value::Number(other) => Value::Number(value / other),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn equal(&self, other: &Value) -> bool {
        self.equal_internal(other, false)
    }

    pub fn strict_equal(&self, other: &Value) -> bool {
        self.equal_internal(other, true)
    }

    pub fn alloc(self, heap: &mut Heap) -> HeapRef {
        heap.alloc(self)
    }
}

#[derive(Debug)]
enum Callable {
    FromAST(Vec<String>, Statement, Rc<RefCell<RsxExecutionContext>>),
    Native,
}

#[derive(Debug)]
pub struct ObjectProperty {
    heap_ref: HeapRef,
}

impl ObjectProperty {
    pub fn new(heap_ref: HeapRef) -> Self {
        ObjectProperty { heap_ref }
    }
}

#[derive(Debug)]
struct Object {
    properties: HashMap<String, ObjectProperty>,
    callable: Option<Callable>,
}

impl Object {
    fn new() -> Self {
        Self {
            callable: None,
            properties: HashMap::new(),
        }
    }

    fn new_function_from_ast(
        args: Vec<String>,
        statement: Box<Statement>,
        captured_context: Rc<RefCell<RsxExecutionContext>>,
    ) -> Self {
        Self {
            callable: Some(Callable::FromAST(args, *statement, captured_context)),
            properties: HashMap::new(),
        }
    }

    fn try_callable(&self) -> Result<&Callable, RsxError> {
        match &self.callable {
            Some(callable) => Ok(callable),
            None => Err(rsx_err!("Failed to cast object {self:#?} as callable")),
        }
    }
}

#[derive(Debug)]
struct RsxExecutionContext {
    parent: Option<Rc<RefCell<RsxExecutionContext>>>,
    variables: HashMap<String, HeapRef>,
}

impl RsxExecutionContext {
    fn new_root() -> Rc<RefCell<RsxExecutionContext>> {
        Rc::new(RefCell::new(Self {
            parent: None,
            variables: HashMap::new(),
        }))
    }
}

#[derive(Clone, Debug)]
pub struct HeapRef {
    address: u64,
}

impl HeapRef {
    fn new(address: u64) -> Self {
        HeapRef { address }
    }

    pub fn value<'a>(&self, rsx: &mut Rsx) -> &mut Value {
        let x = rsx.heap.memory.get(&self.address).unwrap();
        unsafe { &mut **x }
    }
}

pub struct CallFrame {
    return_value: Option<HeapRef>,
}

pub struct Rsx {
    contexts: Vec<Rc<RefCell<RsxExecutionContext>>>,
    call_stack: Vec<CallFrame>,
    pub latest_value: Option<HeapRef>,
    heap: Heap,
}

const GLOBAL_THIS: &str = "globalThis";
const GLOBAL_FALSE: &str = "false";
const GLOBAL_TRUE: &str = "true";
const GLOBAL_UNDEFINED: &str = "undefined";
const GLOBAL_NULL: &str = "null";

impl Rsx {
    pub fn new() -> Rsx {
        let mut rsx = Rsx {
            call_stack: vec![],
            heap: Heap::new(),
            latest_value: None,
            contexts: vec![],
        };

        let global_context = Rc::new(RefCell::new(RsxExecutionContext {
            parent: None,
            variables: HashMap::new(),
        }));

        global_context
            .borrow_mut()
            .variables
            .insert(GLOBAL_THIS.to_string(), rsx.heap.alloc_object());

        rsx.contexts.push(global_context);

        let true_ = rsx.heap.get_true();
        rsx.declare_global(GLOBAL_TRUE, true_);
        let false_ = rsx.heap.get_false();
        rsx.declare_global(GLOBAL_FALSE, false_);
        let undefined = rsx.heap.get_undefined();
        rsx.declare_global(GLOBAL_UNDEFINED, undefined);
        let null = rsx.heap.get_null();
        rsx.declare_global(GLOBAL_NULL, null);

        rsx
    }

    pub fn execute_program(&mut self, program: Program) -> Result<(), RsxError> {
        for statement in &program.statements {
            self.execute_statement(&statement)?;
        }

        Ok(())
    }

    fn get_global_context(&mut self) -> Rc<RefCell<RsxExecutionContext>> {
        self.contexts.get(0).unwrap().clone()
    }

    fn declare_global(&mut self, name: &str, heap_ref: HeapRef) {
        self.get_global_context()
            .borrow_mut()
            .variables
            .get(GLOBAL_THIS)
            // SAFE, created ^
            .unwrap()
            .value(self)
            .try_object()
            // SAFE, created ^
            .unwrap()
            .properties
            .insert(name.to_string(), ObjectProperty::new(heap_ref));
    }

    fn execute_expression(&mut self, expression: &Expression) -> Result<HeapRef, RsxError> {
        let result = match expression {
            Expression::Num(value) => self.heap.alloc_number(*value),
            Expression::String(value) => self.heap.alloc_string(value.clone()),
            Expression::Identifier(name) => self
                .get_variable(name)
                .ok_or(rsx_err!("{name} is not defined."))?,
            Expression::Unary(expression, operator) => {
                let value = self
                    .execute_expression(expression)?
                    .value(self)
                    .try_number()?;

                let result = match operator {
                    UnaryOperator::Minus => -value,
                    UnaryOperator::Plus => {
                        if value.is_sign_positive() {
                            value
                        } else {
                            -value
                        }
                    }
                    _ => unimplemented!(),
                };

                self.heap.alloc_number(result)
            }
            Expression::Binary(left, operator, right) => {
                let left = self.execute_expression(&left)?;
                let left = left.value(self);
                let right = self.execute_expression(&right)?;
                let right = right.value(self);

                let result = match operator {
                    BinaryOperator::Add => left.add(right),
                    BinaryOperator::Sub => left.sub(right),
                    BinaryOperator::Multiply => left.multiply(right),
                    BinaryOperator::Div => left.div(&right),
                    BinaryOperator::Equal => Value::Boolean(left.equal(&right)),
                    BinaryOperator::StrictEqual => Value::Boolean(left.strict_equal(&right)),
                    _ => unimplemented!(),
                };

                self.heap.alloc(result)
            }
            Expression::Call(function, args) => {
                let args = {
                    let mut collected = vec![];
                    for arg in args {
                        collected.push(self.execute_expression(arg)?);
                    }

                    collected
                };

                let function = self.execute_expression(&function)?;
                let callable = function.value(self).try_object()?.try_callable()?;

                let return_value = match callable {
                    Callable::FromAST(arg_names, statement, captured_context) => {
                        self.spawn_execution_context(captured_context.clone());

                        for (arg_index, arg_name) in arg_names.iter().enumerate() {
                            if let Some(arg_value) = args.get(arg_index) {
                                self.current_context()
                                    .borrow_mut()
                                    .variables
                                    .insert(arg_name.clone(), arg_value.clone());
                            }
                        }

                        self.call_stack.push(CallFrame { return_value: None });
                        self.execute_statement(statement)?;
                        self.pop_execution_context();
                        self.call_stack.pop().unwrap().return_value
                    }
                    _ => unimplemented!(),
                };

                if let Some(return_value) = return_value {
                    return_value
                } else {
                    self.heap.get_undefined()
                }
            }
            _ => todo!(),
        };

        self.latest_value = Some(result.clone());

        Ok(result)
    }

    pub fn execute_statement(&mut self, statement: &Statement) -> Result<(), RsxError> {
        match statement {
            Statement::Expression(expression) => {
                self.execute_expression(expression)?;
            }
            Statement::Function(name, args, body) => {
                if self.current_context().borrow().variables.contains_key(name) {
                    return Err(rsx_err!(
                        "Variable {name} already exists in the current scope"
                    ));
                }

                let curr_context = self.current_context().clone();

                let function = self.heap.alloc(Value::Object(Object::new_function_from_ast(
                    args.clone(),
                    body.clone(),
                    curr_context,
                )));

                self.current_context()
                    .borrow_mut()
                    .variables
                    .insert(name.to_string(), function);
            }
            Statement::Let(name, expression) => {
                let value = self.execute_expression(expression)?;

                if self.current_context().borrow().variables.contains_key(name) {
                    return Err(rsx_err!(
                        "Variable {name} already exists in the current scope"
                    ));
                }

                self.current_context()
                    .borrow_mut()
                    .variables
                    .insert(name.to_string(), value);
            }
            Statement::Return(expression) => {
                if self.call_stack.last_mut().is_some() {
                    self.call_stack.last_mut().unwrap().return_value =
                        Some(self.execute_expression(expression)?)
                } else {
                    return Err(rsx_err!(
                        "Return statement is allowed only within function call"
                    ));
                }
            }
            Statement::Block(statements) => {
                self.spawn_execution_context_from_current();

                for statement in statements {
                    self.execute_statement(statement)?;

                    if matches!(statement, Statement::Return(_)) {
                        break;
                    }
                }

                self.pop_execution_context();
            }
            Statement::Assign(value, expr) => {
                match value.as_ref() {
                    Expression::Identifier(identifier) => match identifier.as_str() {
                        "null" | "false" | "NaN" | "undefined" => {
                            return Err(rsx_err!("Assign to {} is forbidden", identifier.as_str()));
                        }
                        name => {
                            {
                                let execution_context = self
                                    .find_variable_execution_context(name)
                                    .ok_or(rsx_err!("Variable {name} is not defined"))?;

                                let value = self.execute_expression(expr)?;
                                execution_context
                                    .borrow_mut()
                                    .variables
                                    .insert(name.to_string(), value);
                            };
                        }
                    },
                    Expression::ElementAccess(..) => {
                        unimplemented!("Element access assign is not implemented")
                    }
                    _ => return Err(rsx_err!("Invalid left-hand side in assignment")),
                };
            }
            _ => todo!("{statement:#?} is not implemented"),
        };
        Ok(())
    }

    fn spawn_execution_context(&mut self, execution_context: Rc<RefCell<RsxExecutionContext>>) {
        let ctx = Rc::new(RefCell::new(RsxExecutionContext {
            parent: Some(execution_context),
            variables: HashMap::new(),
        }));

        self.contexts.push(ctx);
    }

    fn spawn_execution_context_from_current(&mut self) {
        let ctx = Rc::new(RefCell::new(RsxExecutionContext {
            parent: Some(self.current_context()),
            variables: HashMap::new(),
        }));

        self.contexts.push(ctx.clone());
    }

    fn pop_execution_context(&mut self) {
        self.contexts.pop();
    }

    fn current_context(&mut self) -> Rc<RefCell<RsxExecutionContext>> {
        self.contexts.last_mut().unwrap().clone()
    }

    fn get_variable(&mut self, name: &str) -> Option<HeapRef> {
        {
            let mut curr_ctx = self.current_context();

            loop {
                if let Some(var) = curr_ctx.borrow().variables.get(name) {
                    return Some(var.clone());
                }

                let parent_context = curr_ctx.borrow().parent.clone();

                if let Some(parent_context) = parent_context {
                    curr_ctx = parent_context.clone();
                } else {
                    break;
                }
            }
        }

        // Try to get from globalThis
        if let Some(var) = self
            .get_global_context()
            .borrow()
            .variables
            .get(GLOBAL_THIS)
            .unwrap()
            .value(self)
            .try_object()
            .unwrap()
            .properties
            .get(name)
        {
            Some(var.heap_ref.clone())
        } else {
            None
        }
    }

    fn find_variable_execution_context(
        &mut self,
        name: &str,
    ) -> Option<Rc<RefCell<RsxExecutionContext>>> {
        {
            let mut curr_ctx = self.current_context();

            loop {
                if curr_ctx.borrow().variables.contains_key(name) {
                    println!("FOUND");
                    return Some(curr_ctx.clone());
                }

                let parent_context = curr_ctx.borrow().parent.clone();

                if let Some(parent_context) = parent_context {
                    curr_ctx = parent_context.clone();
                } else {
                    break;
                }
            }
        }

        None
    }
}
