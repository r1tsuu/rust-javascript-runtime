#[cfg(test)]
use crate::parser::program_parser;
#[cfg(test)]
use crate::rsx::*;

#[cfg(test)]

fn run_test(source: &str) -> HeapRef {
    use chumsky::Parser;

    let program = program_parser()
        .parse(source)
        .map_err(|err| {
            println!("{err:#?}");
            err
        })
        .unwrap();
    let mut rsx = Rsx::new();
    rsx.execute_program(program)
        .map_err(|err| {
            println!("{err:#?}");
            err
        })
        .unwrap();
    rsx.latest_heap_ref.clone()
}

#[test]
fn core() {
    assert_eq!(
        run_test("100+200*3+5+(3+5*3+6);")
            .value()
            .try_number()
            .unwrap(),
        729.0
    );
    assert_eq!(run_test("100 == 100;").value().try_boolean().unwrap(), true);
    assert_eq!(
        run_test("100 == \"100\";").value().try_boolean().unwrap(),
        true
    );
    assert_eq!(
        run_test("100 === 100;").value().try_boolean().unwrap(),
        true
    );
    assert_eq!(
        run_test("100 === \"100\";").value().try_boolean().unwrap(),
        false
    );
    assert_eq!(
        run_test("let x = 1; let b = 6; x + b;")
            .value()
            .try_number()
            .unwrap(),
        7.0
    );
    assert_eq!(
        run_test("let x = 1; let b = 6; x = 10 + b; x;")
            .value()
            .try_number()
            .unwrap(),
        16.0
    );
    assert_eq!(
        run_test("\"Hello World\";").value().try_string().unwrap(),
        "Hello World"
    );
    assert_eq!(
        run_test("function x(a, b) { return a + b; } x(2, 3);")
            .value()
            .try_number()
            .unwrap(),
        5.0
    );
    // ASSERT EQUAL FOR FUNCTIONS BY REF
    assert_eq!(
        run_test("function x(a, b) { return a + b; } x == x;")
            .value()
            .try_boolean()
            .unwrap(),
        true
    );
    // ASSERT EQUAL FOR FUNCTIONS BY REF
    assert_eq!(
        run_test("function x(a, b) { return a + b; } function y(a, b) { return a + b; } x==y;")
            .value()
            .try_boolean()
            .unwrap(),
        false
    );

    // REFACTOR TODO:
    // let result =
    //     ExpressionEvaluator::evaluate_source("let x = {a: 10, b: \"Hello World\"}").unwrap();
    // let obj = JSObject::cast(result.as_ref()).unwrap();
    // assert_eq!(obj.get_key("a").cast_to_number().value, 10.0);
    // assert_eq!(obj.get_key("b").cast_to_string().value, "Hello World");

    // let result =
    //     ExpressionEvaluator::evaluate_source("let x = {a: 10, b: \"Hello World\"}; x.b").unwrap();
    // assert_eq!(result.cast_to_string().value, "Hello World");

    // let result = ExpressionEvaluator::evaluate_source("globalThis").unwrap();
    // // assert globalThis
    // JSObject::cast(result.as_ref()).unwrap();
}

// TODO:
// #[test]
// fn core_addons() {
//     let result = ExpressionEvaluator::evaluate_source("Math.sqrt(4)").unwrap();
//     assert_eq!(result.cast_to_number().value, 2.0);

//     let result = ExpressionEvaluator::evaluate_source("Math.pow(2, 3)").unwrap();
//     assert_eq!(result.cast_to_number().value, 8.0);

//     let result = ExpressionEvaluator::evaluate_source("Math.abs(-10)").unwrap();
//     assert_eq!(result.cast_to_number().value, 10.0);
// }
