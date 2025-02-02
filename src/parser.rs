use chumsky::{combinator::Repeated, prelude::*};

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Div,
    Multiply,
    Equal,
    StrictEqual,
    NotEqual,
    NotStrictEqual,
    And,
    Or,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Minus,
    Plus,
    Typeof,
    Not,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Num(f64),
    String(String),
    Identifier(String),
    Unary(Box<Expression>, UnaryOperator),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Negative(Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Array(Vec<Expression>),
    FunctionExpression(Option<String>, Vec<String>, Box<Statement>),
    /// Object Expression, Element expression
    ElementAccess(Box<Expression>, Box<Expression>),
    /// Key expression, Value expression
    Object(Vec<(Expression, Expression)>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(String, Box<Expression>),
    Assign(Box<Expression>, Box<Expression>),
    Return(Box<Expression>),
    Expression(Box<Expression>),
    Block(Vec<Statement>),
    Function(String, Vec<String>, Box<Statement>),
    Condition(
        (Box<Expression>, Box<Statement>),
        Option<Vec<(Box<Expression>, Box<Statement>)>>,
        Option<Box<Statement>>,
    ),
}

enum FuncCallArgsOrProperty {
    Arguments(Vec<Expression>),
    Property(Expression),
}

const LET: &str = "let";
const IF: &str = "if";
const ELSE: &str = "else";
const ELSE_IF: &str = "else if";
const FUNCTION: &str = "function";
const RETURN: &str = "return";
const KEYWORDS: &[&str] = &[LET, IF, ELSE, ELSE_IF, FUNCTION, RETURN];

const BRACKET_OPEN: char = '[';
const BRACKET_CLOSE: char = ']';
const PAREN_OPEN: char = '(';
const PAREN_CLOSE: char = ')';
const BRACE_OPEN: char = '{';
const BRACE_CLOSE: char = '}';
const SEMICOLON: char = ';';
const DOT: char = '.';
const COMMA: char = ',';
const COLON: char = ':';
const DOUBLE_QUOTE: char = '"';
const ASSIGN: char = '=';

const NOT: &str = "!";
const TYPEOF: &str = "typeof";
const MINUS: &str = "-";
const PLUS: &str = "+";
const MULTIPLY: &str = "*";
const DIVIDE: &str = "/";
const EQUAL: &str = "==";
const STRICT_EQUAL: &str = "===";
const NOT_EQUAL: &str = "!=";
const NOT_STRICT_EQUAL: &str = "!==";
const AND: &str = "&&";
const OR: &str = "||";
const LESS_THAN: &str = "<";
const LESS_THAN_OR_EQUAL: &str = "<=";
const GREATER_THAN: &str = ">";
const GREATER_THAN_OR_EQUAL: &str = ">=";
const ARROW: &str = "=>";
const COMMENT_SINGLE_LINE: &str = "//";
const COMMENT_MULTI_LINE_START: &str = "/*";
const COMMENT_MULTI_LINE_END: &str = "*/";

fn number_parser() -> impl Parser<char, Expression, Error = Simple<char>> + Clone {
    text::int(10)
        .map(|s: String| Expression::Num(s.parse().unwrap()))
        .padded()
        .or(text::int(10)
            .then_ignore(just(DOT))
            .then(text::int(10))
            .map(|(s1, s2)| Expression::Num(format!("{s1}.{s2}").parse().unwrap())))
        .or(text::keyword("NaN").map(|()| Expression::Num(f64::NAN)))
        .or(text::keyword("Infinity").map(|()| Expression::Num(f64::INFINITY)))
}

fn string_parser() -> impl Parser<char, Expression, Error = Simple<char>> + Clone {
    let escaped_char = just('\\').ignore_then(choice((
        just('\\'),
        just('\"'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
    )));

    just(DOUBLE_QUOTE)
        .ignore_then(
            filter(|c| *c != '\"' && *c != '\\')
                .or(escaped_char)
                .repeated(),
        )
        .then_ignore(just(DOUBLE_QUOTE))
        .collect::<String>()
        .map(Expression::String)
        .padded()
}

fn identifier_parser() -> impl Parser<char, Expression, Error = Simple<char>> + Clone {
    text::ident()
        .try_map(|ident: String, span| {
            if KEYWORDS.contains(&ident.as_str()) {
                Err(Simple::custom(span, format!("Unexpected keyword: {ident}")))
            } else {
                Ok(Expression::Identifier(ident))
            }
        })
        .padded()
}

fn array_parser(
    expr: impl Parser<char, Expression, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expression, Error = Simple<char>> + Clone {
    expr.padded()
        .separated_by(just(COMMA).padded())
        .delimited_by(just(BRACKET_OPEN).padded(), just(BRACE_CLOSE).padded())
        .map(Expression::Array)
}

fn object_parser(
    expr: impl Parser<char, Expression, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expression, Error = Simple<char>> + Clone {
    text::ident()
        .padded()
        .map(Expression::String)
        .or(text::int(10).padded().map(Expression::String))
        .or(expr
            .clone()
            .padded()
            .delimited_by(just(BRACKET_OPEN), just(BRACKET_CLOSE)))
        .padded()
        .then_ignore(just(COLON).padded())
        .then(expr.clone())
        .separated_by(just(COMMA).padded())
        .delimited_by(just(BRACE_OPEN).padded(), just(BRACE_CLOSE).padded())
        .map(Expression::Object)
}

fn named_function_base_parser(
    stmt_parser: impl Parser<char, Statement, Error = Simple<char>> + Clone,
) -> impl Parser<char, ((String, Vec<String>), Statement), Error = Simple<char>> + Clone {
    text::keyword(FUNCTION)
        .padded()
        .ignore_then(text::ident())
        .padded()
        .then(
            text::ident()
                .separated_by(just(COMMA).padded())
                .delimited_by(just(PAREN_OPEN), just(PAREN_CLOSE)),
        )
        .then(stmt_parser.padded())
}

fn block_parser(
    stmt_parser: impl Parser<char, Statement, Error = Simple<char>> + Clone,
) -> impl Parser<char, Statement, Error = Simple<char>> + Clone {
    stmt_parser
        .clone()
        .repeated()
        .delimited_by(just(BRACE_OPEN).padded(), just(BRACE_CLOSE).padded())
        .padded()
        .map(Statement::Block)
}

fn function_expression_parser(
    stmt_parser: impl Parser<char, Statement, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expression, Error = Simple<char>> + Clone {
    let func_declr_expr =
        named_function_base_parser(stmt_parser.clone()).map(|((name, args), block)| {
            Expression::FunctionExpression(Some(name), args, Box::new(block))
        });

    let arrow_func_expr = text::ident()
        .separated_by(just(COMMA).padded())
        .delimited_by(just(PAREN_OPEN), just(PAREN_CLOSE))
        .padded()
        .then_ignore(just(ARROW))
        .then(block_parser(stmt_parser.clone()).padded())
        .map(|(args, block)| Expression::FunctionExpression(None, args, Box::new(block)));

    let unnamed_func_expr = text::keyword(FUNCTION)
        .padded()
        .then(
            text::ident()
                .separated_by(just(COMMA).padded())
                .delimited_by(just(PAREN_OPEN), just(PAREN_CLOSE)),
        )
        .then(stmt_parser.clone().padded())
        .map(|((_, args), block)| Expression::FunctionExpression(None, args, Box::new(block)));

    choice((func_declr_expr, unnamed_func_expr, arrow_func_expr))
}

fn atom_parser(
    expr_parser: impl Parser<char, Expression, Error = Simple<char>> + Clone,
    stmt_parser: impl Parser<char, Statement, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expression, Error = Simple<char>> + Clone {
    let atom = choice((
        expr_parser
            .clone()
            .delimited_by(just(PAREN_OPEN), just(PAREN_CLOSE)),
        function_expression_parser(stmt_parser),
        number_parser(),
        identifier_parser(),
        string_parser(),
        array_parser(expr_parser.clone()),
        object_parser(expr_parser.clone()),
    ));

    let property = just(DOT)
        .ignore_then(text::ident().map(Expression::String))
        .or(expr_parser
            .clone()
            .delimited_by(just(BRACKET_OPEN), just(BRACKET_CLOSE)))
        .map(|prop| (FuncCallArgsOrProperty::Property(prop)));

    // Function calls - now returns (bool, Expression) instead of (bool, Vec<Expression>)
    let args = expr_parser
        .clone()
        .padded()
        .separated_by(just(COMMA).padded())
        .delimited_by(just(PAREN_OPEN), just(PAREN_CLOSE))
        .map(|args| (FuncCallArgsOrProperty::Arguments(args))); // Wrap the Vec in an Expression variant

    // Combined member expression
    atom.clone()
        .then(property.or(args).repeated())
        .foldl(|expr, right| match right {
            FuncCallArgsOrProperty::Arguments(args) => Expression::Call(Box::new(expr), args),
            FuncCallArgsOrProperty::Property(prop) => {
                Expression::ElementAccess(Box::new(expr), Box::new(prop))
            }
        })
}

fn binary(
    operator: BinaryOperator,
) -> impl Fn(Box<Expression>, Box<Expression>) -> Expression + Clone {
    move |x, y| Expression::Binary(x, operator.clone(), y)
}

fn unary(operator: UnaryOperator) -> impl Fn(Box<Expression>) -> Expression + Clone {
    move |x| Expression::Unary(x, operator.clone())
}

pub fn expr_parser<'a>(
    stmt_parser: impl Parser<char, Statement, Error = Simple<char>> + Clone + 'a,
) -> impl Parser<char, Expression, Error = Simple<char>> + Clone + 'a {
    recursive(|expr_parser| {
        let op = |c| just(c).padded();

        let unary = op(MINUS)
            .to(unary(UnaryOperator::Minus))
            .or(op(PLUS).to(unary(UnaryOperator::Plus)))
            .or(op(TYPEOF).to(unary(UnaryOperator::Typeof)))
            .or(op(NOT).to(unary(UnaryOperator::Not)))
            .repeated()
            .then(atom_parser(expr_parser, stmt_parser))
            .foldr(|op, rhs| op(Box::new(rhs)));

        let product = unary
            .clone()
            .then(
                op(MULTIPLY)
                    .to(binary(BinaryOperator::Multiply))
                    .or(op(DIVIDE).to(binary(BinaryOperator::Div)))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                op(PLUS)
                    .to(binary(BinaryOperator::Add))
                    .or(op(MINUS).to(binary(BinaryOperator::Sub)))
                    .or(op(STRICT_EQUAL).to(binary(BinaryOperator::StrictEqual)))
                    .or(op(NOT_EQUAL).to(binary(BinaryOperator::NotEqual)))
                    .or(op(NOT_STRICT_EQUAL).to(binary(BinaryOperator::NotStrictEqual)))
                    .or(op(AND).to(binary(BinaryOperator::And)))
                    .or(op(OR).to(binary(BinaryOperator::Or)))
                    .or(op(LESS_THAN).to(binary(BinaryOperator::LessThan)))
                    .or(op(LESS_THAN_OR_EQUAL).to(binary(BinaryOperator::LessThanOrEqual)))
                    .or(op(GREATER_THAN).to(binary(BinaryOperator::GreaterThan)))
                    .or(op(GREATER_THAN_OR_EQUAL).to(binary(BinaryOperator::GreaterThanOrEqual)))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let condition = sum
            .clone()
            .then(
                op(EQUAL)
                    .to(binary(BinaryOperator::Equal))
                    .or(op(STRICT_EQUAL).to(binary(BinaryOperator::StrictEqual)))
                    .or(op(NOT_EQUAL).to(binary(BinaryOperator::NotEqual)))
                    .or(op(NOT_STRICT_EQUAL).to(binary(BinaryOperator::NotStrictEqual)))
                    .or(op(AND).to(binary(BinaryOperator::And)))
                    .or(op(OR).to(binary(BinaryOperator::Or)))
                    .or(op(LESS_THAN).to(binary(BinaryOperator::LessThan)))
                    .or(op(LESS_THAN_OR_EQUAL).to(binary(BinaryOperator::LessThanOrEqual)))
                    .or(op(GREATER_THAN).to(binary(BinaryOperator::GreaterThan)))
                    .or(op(GREATER_THAN_OR_EQUAL).to(binary(BinaryOperator::GreaterThanOrEqual)))
                    .then(sum)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        condition
    })
}

fn condition_parser<'a>(
    stmt_parser: impl Parser<char, Statement, Error = Simple<char>> + Clone + 'a,
) -> impl Parser<char, Statement, Error = Simple<char>> + Clone + 'a {
    let else_clause = text::keyword(ELSE)
        .padded()
        .ignore_then(block_parser(stmt_parser.clone()));

    let else_if_clauses = just(ELSE_IF)
        .padded()
        .ignore_then(
            expr_parser(stmt_parser.clone())
                .delimited_by(just(PAREN_OPEN).padded(), just(PAREN_CLOSE).padded()),
        )
        .then(block_parser(stmt_parser.clone()))
        .repeated()
        .at_least(1);

    text::keyword(IF)
        .padded()
        .ignore_then(
            expr_parser(stmt_parser.clone())
                .delimited_by(just(PAREN_OPEN).padded(), just(PAREN_CLOSE).padded()),
        )
        .then(block_parser(stmt_parser.clone()))
        .then(else_if_clauses.or_not())
        .then(else_clause.or_not())
        .map(|(((if_expr, if_stmt), else_if_clauses), else_clause)| {
            let else_if_clauses = else_if_clauses.map(|v| {
                Vec::from_iter(
                    v.into_iter()
                        .map(|(expr, stmt)| (Box::new(expr), Box::new(stmt))),
                )
            });

            Statement::Condition(
                (Box::new(if_expr), Box::new(if_stmt)),
                else_if_clauses,
                else_clause.map(Box::new),
            )
        })
}

fn comment_ignore_parser() -> Repeated<impl Parser<char, (), Error = Simple<char>> + Clone> {
    let single_line = just(COMMENT_SINGLE_LINE)
        .then(take_until(text::newline()))
        .ignored();
    let multi_line = just(COMMENT_MULTI_LINE_START)
        .then(take_until(just(COMMENT_MULTI_LINE_END)))
        .ignored();

    choice((single_line, multi_line)).repeated()
}

fn stmt_parser() -> impl Parser<char, Statement, Error = Simple<char>> {
    recursive(|stmt_parser| {
        let expr_end = just(SEMICOLON).or(just('\n'));

        let let_stmt = text::keyword(LET)
            .padded()
            .ignore_then(text::ident().padded())
            .then_ignore(just(ASSIGN).padded())
            .then(expr_parser(stmt_parser.clone()))
            .then_ignore(expr_end)
            .map(|(name, expr)| Statement::Let(name, Box::new(expr)));

        let assign_stmt = expr_parser(stmt_parser.clone())
            .padded()
            .then_ignore(just(ASSIGN).padded())
            .then(expr_parser(stmt_parser.clone()))
            .then_ignore(expr_end)
            .try_map(|(name, expr), span| match name {
                Expression::Identifier(_) | Expression::ElementAccess(..) => {
                    Ok(Statement::Assign(Box::new(name), Box::new(expr)))
                }
                _ => Err(Simple::custom(
                    span,
                    "Invalid left-hand side in assigment. Must be a reference.",
                )),
            });

        let return_stmt = text::keyword(RETURN)
            .padded()
            .ignore_then(expr_parser(stmt_parser.clone()))
            .then_ignore(expr_end)
            .map(|x| Statement::Return(Box::new(x)));

        let func_stmt = named_function_base_parser(stmt_parser.clone())
            .map(|((name, args), block)| Statement::Function(name, args, Box::new(block)));

        let expr_stmt = expr_parser(stmt_parser.clone())
            .then_ignore(expr_end)
            .map(|x| Statement::Expression(Box::new(x)));

        choice((
            let_stmt,
            return_stmt,
            condition_parser(stmt_parser.clone()),
            func_stmt,
            assign_stmt,
            block_parser(stmt_parser),
            expr_stmt,
        ))
        .padded()
        .padded_by(comment_ignore_parser())
    })
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    fn new(statements: Vec<Statement>) -> Self {
        Program { statements }
    }
}

pub fn program_parser() -> impl Parser<char, Program, Error = Simple<char>> {
    stmt_parser()
        .padded()
        .padded_by(comment_ignore_parser())
        .repeated()
        .then_ignore(end())
        .map(Program::new)
}
