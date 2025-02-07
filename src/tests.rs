#[cfg(test)]
use crate::parser::program_parser;
#[cfg(test)]
use crate::rsx::*;

#[cfg(test)]

fn run_test(rsx: &mut Rsx, source: &str) -> HeapRef {
    // reset
    *rsx = Rsx::new();

    use chumsky::Parser;

    let program = program_parser()
        .parse(source)
        .map_err(|err| {
            println!("{err:#?}");
            err
        })
        .unwrap();

    rsx.execute_program(program)
        .map_err(|err| {
            println!("{err:#?}");
            err
        })
        .unwrap();

    rsx.latest_value.clone().unwrap()
}

#[test]
fn core() {
    let rsx = &mut Rsx::new();

    assert_eq!(
        run_test(rsx, "100+200*3+5+(3+5*3+6);")
            .value(rsx)
            .try_number()
            .unwrap(),
        729.0
    );
    assert_eq!(
        run_test(rsx, "100 == 100;")
            .value(rsx)
            .try_boolean()
            .unwrap(),
        true
    );
    assert_eq!(
        run_test(rsx, "100 == \"100\";")
            .value(rsx)
            .try_boolean()
            .unwrap(),
        true
    );
    assert_eq!(
        run_test(rsx, "100 === 100;")
            .value(rsx)
            .try_boolean()
            .unwrap(),
        true
    );
    assert_eq!(
        run_test(rsx, "100 === \"100\";")
            .value(rsx)
            .try_boolean()
            .unwrap(),
        false
    );
    assert_eq!(
        run_test(rsx, "let x = 1; let b = 6; x + b;")
            .value(rsx)
            .try_number()
            .unwrap(),
        7.0
    );
    assert_eq!(
        run_test(rsx, "let x = 1; let b = 6; x = 10 + b; x;")
            .value(rsx)
            .try_number()
            .unwrap(),
        16.0
    );
    assert_eq!(
        run_test(rsx, "\"Hello World\";")
            .value(rsx)
            .try_string()
            .unwrap(),
        "Hello World"
    );
    assert_eq!(
        run_test(rsx, "function x(a, b) { return a + b; } x(2, 3);")
            .value(rsx)
            .try_number()
            .unwrap(),
        5.0
    );
    // ASSERT EQUAL FOR FUNCTIONS BY REF
    assert_eq!(
        run_test(rsx, "function x(a, b) { return a + b; } x == x;")
            .value(rsx)
            .try_boolean()
            .unwrap(),
        true
    );
    // ASSERT EQUAL FOR FUNCTIONS BY REF
    assert_eq!(
        run_test(
            rsx,
            "function x(a, b) { return a + b; } function y(a, b) { return a + b; } x==y;"
        )
        .value(rsx)
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
