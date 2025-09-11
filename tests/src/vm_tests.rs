#![allow(dead_code)]
#![cfg(test)]
use std::collections::HashMap;

use crate::utils::prepare_program_for_test;
use compiler::Compiler;
use object::{HashPair, Object, ObjectTraits};
use vm::{VM, VMError};

fn test_integer_object(expected: Object, actual: Object) -> Result<(), String> {
    assert_eq!(expected, actual);
    Ok(())
}

#[derive(Debug)]
struct VmTestCase {
    input: &'static str,
    expected: Result<Object, VMError>,
}

fn run_vm_tests(tests: Vec<VmTestCase>) {
    for tt in tests {
        let program = prepare_program_for_test(tt.input);
        let mut comp = Compiler::new();
        comp.compile(program).unwrap();
        let mut vm = VM::new(comp.bytecode());
        let result = vm.run();
        let stack_elem = vm.last_popped_stack_elem().unwrap().clone();
        match tt.expected {
            Err(err) => {
                assert_eq!(result.unwrap_err(), err);
            }
            Ok(expected) => {
                test_expected_object(expected, stack_elem);
            }
        }
    }
}

fn test_expected_object(expected: Object, actual: Object) {
    match (&expected, &actual) {
        (
            Object::Integer {
                value: expected_value,
            },
            Object::Integer {
                value: actual_value,
            },
        ) => {
            assert_eq!(expected_value, actual_value);
        }
        (
            Object::Boolean {
                value: expected_value,
            },
            Object::Boolean {
                value: actual_value,
            },
        ) => {
            assert_eq!(expected_value, actual_value);
        }
        (Object::Null, Object::Null) => {}
        (
            Object::String {
                value: expected_value,
            },
            Object::String {
                value: actual_value,
            },
        ) => {
            assert_eq!(expected_value, actual_value);
        }
        (Object::Array { elements: expected }, Object::Array { elements: actual }) => {
            assert_eq!(expected.len(), actual.len());

            for (i, _) in expected.iter().enumerate() {
                test_expected_object(expected[i].clone(), actual[i].clone());
            }
        }
        (Object::Hash { pairs: expected }, Object::Hash { pairs: actual }) => {
            assert_eq!(expected.len(), actual.len());
            for (key, _) in expected.iter() {
                assert_eq!(expected.get(key), actual.get(key));
            }
        }
        _ => panic!(
            "unexpected object value {:?}, expected={:?}",
            actual, expected
        ),
    }
}

#[test]
fn test_integer_arithmetic() {
    let tests = vec![
        VmTestCase {
            input: "1",
            expected: Ok(Object::Integer { value: 1 }),
        },
        VmTestCase {
            input: "2",
            expected: Ok(Object::Integer { value: 2 }),
        },
        VmTestCase {
            input: "1 + 2",
            expected: Ok(Object::Integer { value: 3 }),
        },
        VmTestCase {
            input: "1 - 2",
            expected: Ok(Object::Integer { value: -1 }),
        },
        VmTestCase {
            input: "1 * 2",
            expected: Ok(Object::Integer { value: 2 }),
        },
        VmTestCase {
            input: "4 / 2",
            expected: Ok(Object::Integer { value: 2 }),
        },
        VmTestCase {
            input: "50 / 2 * 2 + 10 - 5",
            expected: Ok(Object::Integer { value: 55 }),
        },
        VmTestCase {
            input: "5 + 5 + 5 + 5 - 10",
            expected: Ok(Object::Integer { value: 10 }),
        },
        VmTestCase {
            input: "2 * 2 * 2 * 2 * 2",
            expected: Ok(Object::Integer { value: 32 }),
        },
        VmTestCase {
            input: "5 * 2 + 10",
            expected: Ok(Object::Integer { value: 20 }),
        },
        VmTestCase {
            input: "5 + 2 * 10",
            expected: Ok(Object::Integer { value: 25 }),
        },
        VmTestCase {
            input: "5 * (2 + 10)",
            expected: Ok(Object::Integer { value: 60 }),
        },
        VmTestCase {
            input: "-5",
            expected: Ok(Object::Integer { value: -5 }),
        },
        VmTestCase {
            input: "-10",
            expected: Ok(Object::Integer { value: -10 }),
        },
        VmTestCase {
            input: "-50 + 100 + -50",
            expected: Ok(Object::Integer { value: 0 }),
        },
        VmTestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: Ok(Object::Integer { value: 50 }),
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_boolean_expression() {
    let tests: Vec<VmTestCase> = vec![
        VmTestCase {
            input: "true",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "false",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "1 < 2",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "1 > 2",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "1 < 1",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "1 > 1",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "1 == 1",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "1 != 1",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "1 == 2",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "1 != 2",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "true == true",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "false == false",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "true == false",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "true != false",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "false != true",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "(1 < 2) == true",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "(1 < 2) == false",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "(1 > 2) == true",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "(1 > 2) == false",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "!true",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "!false",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "!5",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "!!true",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "!!false",
            expected: Ok(Object::Boolean { value: false }),
        },
        VmTestCase {
            input: "!!5",
            expected: Ok(Object::Boolean { value: true }),
        },
        VmTestCase {
            input: "!(if (false) {5;})",
            expected: Ok(Object::Boolean { value: true }),
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_conditionals() {
    let tests = vec![
        VmTestCase {
            input: "if (true) {10}",
            expected: Ok(Object::Integer { value: 10 }),
        },
        VmTestCase {
            input: "if (true) {10} else {20}",
            expected: Ok(Object::Integer { value: 10 }),
        },
        VmTestCase {
            input: "if (false) {10} else {20}",
            expected: Ok(Object::Integer { value: 20 }),
        },
        VmTestCase {
            input: "if (1) {10}",
            expected: Ok(Object::Integer { value: 10 }),
        },
        VmTestCase {
            input: "if (1) {10}",
            expected: Ok(Object::Integer { value: 10 }),
        },
        VmTestCase {
            input: "if (1 < 2) {10} else { 20 }",
            expected: Ok(Object::Integer { value: 10 }),
        },
        VmTestCase {
            input: "if (1 > 2) {10} else { 20 }",
            expected: Ok(Object::Integer { value: 20 }),
        },
        VmTestCase {
            input: "if (1 > 2) { 10 }",
            expected: Ok(Object::Null),
        },
        VmTestCase {
            input: "if (false) { 10 }",
            expected: Ok(Object::Null),
        },
        VmTestCase {
            input: "if ((if (false) { 10 })) { 10 } else { 20 }",
            expected: Ok(Object::Integer { value: 20 }),
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_global_let_statements() {
    let tests = vec![
        VmTestCase {
            input: "let one = 1; one",
            expected: Ok(Object::Integer { value: 1 }),
        },
        VmTestCase {
            input: "let one = 1; let two = 2; one + two;",
            expected: Ok(Object::Integer { value: 3 }),
        },
        VmTestCase {
            input: "let one = 1; let two = one + one; one + two",
            expected: Ok(Object::Integer { value: 3 }),
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_string_expressions() {
    let tests = vec![
        VmTestCase {
            input: "\"monkey\"",
            expected: Ok(Object::String {
                value: "monkey".to_string(),
            }),
        },
        VmTestCase {
            input: "\"mon\" + \"key\" + \"banana\"",
            expected: Ok(Object::String {
                value: "monkeybanana".to_string(),
            }),
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_array_literals() {
    let tests = vec![
        VmTestCase {
            input: "[]",
            expected: Ok(Object::Array {
                elements: Vec::new(),
            }),
        },
        VmTestCase {
            input: "[1, 2, 3]",
            expected: Ok(Object::Array {
                elements: vec![
                    Object::Integer { value: 1 },
                    Object::Integer { value: 2 },
                    Object::Integer { value: 3 },
                ],
            }),
        },
        VmTestCase {
            input: "[1 + 2, 3 * 4, 5 + 6]",
            expected: Ok(Object::Array {
                elements: vec![
                    Object::Integer { value: 3 },
                    Object::Integer { value: 12 },
                    Object::Integer { value: 11 },
                ],
            }),
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_hash_literal() {
    let tests = vec![
        VmTestCase {
            input: "{}",
            expected: Ok(Object::Hash {
                pairs: HashMap::new(),
            }),
        },
        VmTestCase {
            input: "{1:2, 2:3}",
            expected: Ok(Object::Hash {
                pairs: HashMap::from([
                    (
                        Object::Integer { value: 1 }.hash().unwrap(),
                        HashPair {
                            key: Object::Integer { value: 1 },
                            value: Object::Integer { value: 2 },
                        },
                    ),
                    (
                        Object::Integer { value: 2 }.hash().unwrap(),
                        HashPair {
                            key: Object::Integer { value: 2 },
                            value: Object::Integer { value: 3 },
                        },
                    ),
                ]),
            }),
        },
        VmTestCase {
            input: "{1 + 1: 2*2, 3+3: 4*4}",
            expected: Ok(Object::Hash {
                pairs: HashMap::from([
                    (
                        Object::Integer { value: 2 }.hash().unwrap(),
                        HashPair {
                            key: Object::Integer { value: 2 },
                            value: Object::Integer { value: 4 },
                        },
                    ),
                    (
                        Object::Integer { value: 6 }.hash().unwrap(),
                        HashPair {
                            key: Object::Integer { value: 6 },
                            value: Object::Integer { value: 16 },
                        },
                    ),
                ]),
            }),
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_index_expression() {
    let tests = vec![
        VmTestCase {
            input: "[1, 2, 3][1]",
            expected: Ok(Object::Integer { value: 2 }),
        },
        VmTestCase {
            input: "[1, 2, 3][0 + 2]",
            expected: Ok(Object::Integer { value: 3 }),
        },
        VmTestCase {
            input: "[[1, 1, 1]][0][0]",
            expected: Ok(Object::Integer { value: 1 }),
        },
        VmTestCase {
            input: "[][0]",
            expected: Ok(Object::Null),
        },
        VmTestCase {
            input: "[1, 2, 3][99]",
            expected: Ok(Object::Null),
        },
        VmTestCase {
            input: "[1][-1]",
            expected: Ok(Object::Null),
        },
        VmTestCase {
            input: "{1: 1, 2: 2}[1]",
            expected: Ok(Object::Integer { value: 1 }),
        },
        VmTestCase {
            input: "{1: 1, 2: 2}[2]",
            expected: Ok(Object::Integer { value: 2 }),
        },
        VmTestCase {
            input: "{1: 1}[0]",
            expected: Ok(Object::Null),
        },
        VmTestCase {
            input: "{}[0]",
            expected: Ok(Object::Null),
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_calling_functions_without_arguments() {
    let tests = vec![
        VmTestCase {
            input: "let fivePlusTen = fn() {5 + 10}; fivePlusTen();",
            expected: Ok(Object::Integer { value: 15 }),
        },
        VmTestCase {
            input: "let one = fn() { 1; }; let two = fn() { 2; }; one() + two()",
            expected: Ok(Object::Integer { value: 3 }),
        },
        VmTestCase {
            input: "let a = fn() { 1 }; let b = fn() { a() + 1 }; let c = fn() { b() + 1 }; c();",
            expected: Ok(Object::Integer { value: 3 }),
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_functions_with_return_statement() {
    let tests = vec![
        VmTestCase {
            input: "let earlyExit = fn() { return 99; 100; }; earlyExit();",
            expected: Ok(Object::Integer { value: 99 }),
        },
        VmTestCase {
            input: "let earlyExit = fn() { return 99; return 100; }; earlyExit();",
            expected: Ok(Object::Integer { value: 99 }),
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_functions_without_return_value() {
    let tests = vec![
        VmTestCase {
            input: "let noReturn = fn() { }; noReturn();",
            expected: Ok(Object::Null),
        },
        VmTestCase {
            input: "let noReturn = fn() { }; let noReturnTwo = fn() { noReturn(); }; noReturn(); noReturnTwo();",
            expected: Ok(Object::Null),
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_first_class_functions() {
    let tests = vec![
        VmTestCase {
            input: "let returnsOne = fn() { 1; }; let returnsOneReturner = fn() { returnsOne; }; returnsOneReturner()();",
            expected: Ok(Object::Integer { value: 1 }),
        },
        VmTestCase {
            input: "let returnsOneReturner = fn() { let returnsOne = fn() { 1; }; returnsOne;} returnsOneReturner()();",
            expected: Ok(Object::Integer { value: 1 }),
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_calling_functions_with_bindings() {
    let tests = vec![
        VmTestCase {
            input: "let one = fn() { let one = 1; one }; one();",
            expected: Ok(Object::Integer { value: 1 }),
        },
        VmTestCase {
            input: "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();",
            expected: Ok(Object::Integer { value: 3 }),
        },
        VmTestCase {
            input: "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; let threeAndFour = fn() { let three = 3; let four = 4; three + four; }; oneAndTwo() + threeAndFour();",
            expected: Ok(Object::Integer { value: 10 }),
        },
        VmTestCase {
            input: "let firstFoobar = fn() { let foobar = 50; foobar; }; let secondFoobar = fn() { let foobar = 100; foobar; }; firstFoobar() + secondFoobar();",
            expected: Ok(Object::Integer { value: 150 }),
        },
        VmTestCase {
            input: "let globalSeed = 50; let minusOne = fn() { let num = 1; globalSeed - num; }; let minusTwo = fn() { let num = 2; globalSeed - num; }; minusOne() + minusTwo();",
            expected: Ok(Object::Integer { value: 97 }),
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_calling_functions_with_arguments_and_bindings() {
    let tests = vec![
        VmTestCase {
            input: "let identity = fn(a) { a; }; identity(4);",
            expected: Ok(Object::Integer { value: 4 }),
        },
        VmTestCase {
            input: "let sum = fn(a, b) { a + b; }; sum(1, 2);",
            expected: Ok(Object::Integer { value: 3 }),
        },
        VmTestCase {
            input: "let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2);",
            expected: Ok(Object::Integer { value: 3 }),
        },
        VmTestCase {
            input: "let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2) + sum(3, 4);",
            expected: Ok(Object::Integer { value: 10 }),
        },
        VmTestCase {
            input: "let sum = fn(a, b) { let c = a + b; c; }; let outer = fn() { sum(1, 2) + sum(3, 4); }; outer();",
            expected: Ok(Object::Integer { value: 10 }),
        },
        VmTestCase {
            input: "let globalNum = 10; let sum = fn(a, b) { let c = a + b; c + globalNum; }; let outer = fn() { sum(1, 2) + sum(3, 4) + globalNum; }; outer() + globalNum;",
            expected: Ok(Object::Integer { value: 50 }),
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_calling_functions_with_wrong_arguments() {
    let tests = vec![
        ("fn() { 1; }(1);", 0, 1),
        ("fn(a) { a; }();", 1, 0),
        ("fn(a, b) { a + b; }(1);", 2, 1),
    ];

    for (input, expected_want, expected_got) in tests {
        let program = prepare_program_for_test(input);
        let mut comp = Compiler::new();
        comp.compile(program).unwrap();
        let mut vm = VM::new(comp.bytecode());
        let result = vm.run();

        match result {
            Err(vm::VMError::WrongArgumentCount { want, got }) => {
                assert_eq!(want, expected_want);
                assert_eq!(got, expected_got);
            }
            Err(other_err) => {
                panic!(
                    "expected WrongArgumentCount error but got: {:?} for input: {}",
                    other_err, input
                );
            }
            Ok(_) => {
                panic!(
                    "expected VM error but resulted in none for input: {}",
                    input
                );
            }
        }
    }
}

#[test]
fn test_builtin_functions() {
    let tests = vec![
        VmTestCase {
            input: "len(\"\")",
            expected: Ok(Object::Integer { value: 0 }),
        },
        VmTestCase {
            input: "puts(\"hello\", \"world\")",
            expected: Ok(Object::Null),
        },
        VmTestCase {
            input: "first([1, 2, 3])",
            expected: Ok(Object::Integer { value: 1 }),
        },
        VmTestCase {
            input: "first([])",
            expected: Ok(Object::Null),
        },
        VmTestCase {
            input: "first(1)",
            expected: Err(VMError::WrongArgumentType {
                want: "ARRAY",
                got: "INTEGER",
            }),
        },
        VmTestCase {
            input: "last([1, 2, 3])",
            expected: Ok(Object::Integer { value: 3 }),
        },
        VmTestCase {
            input: "last([])",
            expected: Ok(Object::Null),
        },
        VmTestCase {
            input: "last(1)",
            expected: Err(VMError::WrongArgumentType {
                want: "ARRAY",
                got: "INTEGER",
            }),
        },
        VmTestCase {
            input: "len(\"four\")",
            expected: Ok(Object::Integer { value: 4 }),
        },
        VmTestCase {
            input: "len(\"hello world\")",
            expected: Ok(Object::Integer { value: 11 }),
        },
        VmTestCase {
            input: "len(1)",
            expected: Err(VMError::WrongArgumentType {
                want: "STRING or ARRAY",
                got: "INTEGER",
            }),
        },
        VmTestCase {
            input: "len(\"one\", \"two\")",
            expected: Err(VMError::WrongArgumentCount { want: 1, got: 2 }),
        },
        VmTestCase {
            input: "len([1, 2, 3])",
            expected: Ok(Object::Integer { value: 3 }),
        },
        VmTestCase {
            input: "len([])",
            expected: Ok(Object::Integer { value: 0 }),
        },
        VmTestCase {
            input: "rest([1, 2, 3])",
            expected: Ok(Object::Array {
                elements: vec![Object::Integer { value: 2 }, Object::Integer { value: 3 }],
            }),
        },
        VmTestCase {
            input: "rest([])",
            expected: Ok(Object::Null),
        },
        VmTestCase {
            input: "push([], 1)",
            expected: Ok(Object::Array {
                elements: vec![Object::Integer { value: 1 }],
            }),
        },
        VmTestCase {
            input: "push(1, 1)",
            expected: Err(VMError::WrongArgumentType {
                want: "ARRAY",
                got: "INTEGER",
            }),
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_closure() {
    let tests = vec![
        VmTestCase {
            input: "let newClosure = fn(a) {
fn() { a; };
};
let closure = newClosure(99);
closure();",
            expected: Ok(Object::Integer { value: 99 }),
        },
        VmTestCase {
            input: "let newAdder = fn(a, b) {
fn(c) { a + b + c };
};
let adder = newAdder(1, 2);
adder(8);",
            expected: Ok(Object::Integer { value: 11 }),
        },
        VmTestCase {
            input: "let newAdder = fn(a, b) {
let c = a + b;
fn(d) { c + d };
};
let adder = newAdder(1, 2);
adder(8);",
            expected: Ok(Object::Integer { value: 11 }),
        },
        VmTestCase {
            input: "let newAdderOuter = fn(a, b) {
let c = a + b;
fn(d) {
let e = d + c;
fn(f) { e + f; };
};
};
let newAdderInner = newAdderOuter(1, 2)
let adder = newAdderInner(3);
adder(8);",
            expected: Ok(Object::Integer { value: 14 }),
        },
        VmTestCase {
            input: "let a = 1;
let newAdderOuter = fn(b) {
fn(c) {
fn(d) { a + b + c + d };
};
};
let newAdderInner = newAdderOuter(2)
let adder = newAdderInner(3);
adder(8);",
            expected: Ok(Object::Integer { value: 14 }),
        },
        VmTestCase {
            input: "let newClosure = fn(a, b) {
let one = fn() { a; };
let two = fn() { b; };
fn() { one() + two(); };
};
let closure = newClosure(9, 90);
closure();",
            expected: Ok(Object::Integer { value: 99 }),
        },
    ];
    run_vm_tests(tests);
}
