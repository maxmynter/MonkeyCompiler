#![allow(dead_code)]
#![cfg(test)]
use std::collections::HashMap;

use crate::utils::prepare_program_for_test;
use compiler::Compiler;
use object::{HashPair, Object, ObjectTraits};
use vm::VM;

fn test_integer_object(expected: Object, actual: Object) -> Result<(), String> {
    assert_eq!(expected, actual);
    Ok(())
}

#[derive(Debug)]
struct VmTestCase {
    input: &'static str,
    expected: Object,
}

fn run_vm_tests(tests: Vec<VmTestCase>) {
    for tt in tests {
        let program = prepare_program_for_test(tt.input);
        let mut comp = Compiler::new();
        comp.compile(program).unwrap();
        let mut vm = VM::new(comp.bytecode());
        let _ = vm.run();
        let stack_elem = vm.last_popped_stack_elem().unwrap().clone();
        test_expected_object(tt.expected, stack_elem);
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
            expected: Object::Integer { value: 1 },
        },
        VmTestCase {
            input: "2",
            expected: Object::Integer { value: 2 },
        },
        VmTestCase {
            input: "1 + 2",
            expected: Object::Integer { value: 3 },
        },
        VmTestCase {
            input: "1 - 2",
            expected: Object::Integer { value: -1 },
        },
        VmTestCase {
            input: "1 * 2",
            expected: Object::Integer { value: 2 },
        },
        VmTestCase {
            input: "4 / 2",
            expected: Object::Integer { value: 2 },
        },
        VmTestCase {
            input: "50 / 2 * 2 + 10 - 5",
            expected: Object::Integer { value: 55 },
        },
        VmTestCase {
            input: "5 + 5 + 5 + 5 - 10",
            expected: Object::Integer { value: 10 },
        },
        VmTestCase {
            input: "2 * 2 * 2 * 2 * 2",
            expected: Object::Integer { value: 32 },
        },
        VmTestCase {
            input: "5 * 2 + 10",
            expected: Object::Integer { value: 20 },
        },
        VmTestCase {
            input: "5 + 2 * 10",
            expected: Object::Integer { value: 25 },
        },
        VmTestCase {
            input: "5 * (2 + 10)",
            expected: Object::Integer { value: 60 },
        },
        VmTestCase {
            input: "-5",
            expected: Object::Integer { value: -5 },
        },
        VmTestCase {
            input: "-10",
            expected: Object::Integer { value: -10 },
        },
        VmTestCase {
            input: "-50 + 100 + -50",
            expected: Object::Integer { value: 0 },
        },
        VmTestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: Object::Integer { value: 50 },
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_boolean_expression() {
    let tests: Vec<VmTestCase> = vec![
        VmTestCase {
            input: "true",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "false",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "1 < 2",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "1 > 2",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "1 < 1",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "1 > 1",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "1 == 1",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "1 != 1",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "1 == 2",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "1 != 2",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "true == true",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "false == false",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "true == false",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "true != false",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "false != true",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "(1 < 2) == true",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "(1 < 2) == false",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "(1 > 2) == true",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "(1 > 2) == false",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "!true",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "!false",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "!5",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "!!true",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "!!false",
            expected: Object::Boolean { value: false },
        },
        VmTestCase {
            input: "!!5",
            expected: Object::Boolean { value: true },
        },
        VmTestCase {
            input: "!(if (false) {5;})",
            expected: Object::Boolean { value: true },
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_conditionals() {
    let tests = vec![
        VmTestCase {
            input: "if (true) {10}",
            expected: Object::Integer { value: 10 },
        },
        VmTestCase {
            input: "if (true) {10} else {20}",
            expected: Object::Integer { value: 10 },
        },
        VmTestCase {
            input: "if (false) {10} else {20}",
            expected: Object::Integer { value: 20 },
        },
        VmTestCase {
            input: "if (1) {10}",
            expected: Object::Integer { value: 10 },
        },
        VmTestCase {
            input: "if (1) {10}",
            expected: Object::Integer { value: 10 },
        },
        VmTestCase {
            input: "if (1 < 2) {10} else { 20 }",
            expected: Object::Integer { value: 10 },
        },
        VmTestCase {
            input: "if (1 > 2) {10} else { 20 }",
            expected: Object::Integer { value: 20 },
        },
        VmTestCase {
            input: "if (1 > 2) { 10 }",
            expected: Object::Null,
        },
        VmTestCase {
            input: "if (false) { 10 }",
            expected: Object::Null,
        },
        VmTestCase {
            input: "if ((if (false) { 10 })) { 10 } else { 20 }",
            expected: Object::Integer { value: 20 },
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_global_let_statements() {
    let tests = vec![
        VmTestCase {
            input: "let one = 1; one",
            expected: Object::Integer { value: 1 },
        },
        VmTestCase {
            input: "let one = 1; let two = 2; one + two;",
            expected: Object::Integer { value: 3 },
        },
        VmTestCase {
            input: "let one = 1; let two = one + one; one + two",
            expected: Object::Integer { value: 3 },
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_string_expressions() {
    let tests = vec![
        VmTestCase {
            input: "\"monkey\"",
            expected: Object::String {
                value: "monkey".to_string(),
            },
        },
        VmTestCase {
            input: "\"mon\" + \"key\" + \"banana\"",
            expected: Object::String {
                value: "monkeybanana".to_string(),
            },
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_array_literals() {
    let tests = vec![
        VmTestCase {
            input: "[]",
            expected: Object::Array {
                elements: Vec::new(),
            },
        },
        VmTestCase {
            input: "[1, 2, 3]",
            expected: Object::Array {
                elements: vec![
                    Object::Integer { value: 1 },
                    Object::Integer { value: 2 },
                    Object::Integer { value: 3 },
                ],
            },
        },
        VmTestCase {
            input: "[1 + 2, 3 * 4, 5 + 6]",
            expected: Object::Array {
                elements: vec![
                    Object::Integer { value: 3 },
                    Object::Integer { value: 12 },
                    Object::Integer { value: 11 },
                ],
            },
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_hash_literal() {
    let tests = vec![
        VmTestCase {
            input: "{}",
            expected: Object::Hash {
                pairs: HashMap::new(),
            },
        },
        VmTestCase {
            input: "{1:2, 2:3}",
            expected: Object::Hash {
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
            },
        },
        VmTestCase {
            input: "{1 + 1: 2*2, 3+3: 4*4}",
            expected: Object::Hash {
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
            },
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_index_expression() {
    let tests = vec![
        VmTestCase {
            input: "[1, 2, 3][1]",
            expected: Object::Integer { value: 2 },
        },
        VmTestCase {
            input: "[1, 2, 3][0 + 2]",
            expected: Object::Integer { value: 3 },
        },
        VmTestCase {
            input: "[[1, 1, 1]][0][0]",
            expected: Object::Integer { value: 1 },
        },
        VmTestCase {
            input: "[][0]",
            expected: Object::Null,
        },
        VmTestCase {
            input: "[1, 2, 3][99]",
            expected: Object::Null,
        },
        VmTestCase {
            input: "[1][-1]",
            expected: Object::Null,
        },
        VmTestCase {
            input: "{1: 1, 2: 2}[1]",
            expected: Object::Integer { value: 1 },
        },
        VmTestCase {
            input: "{1: 1, 2: 2}[2]",
            expected: Object::Integer { value: 2 },
        },
        VmTestCase {
            input: "{1: 1}[0]",
            expected: Object::Null,
        },
        VmTestCase {
            input: "{}[0]",
            expected: Object::Null,
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_calling_functions_without_arguments() {
    let tests = vec![
        VmTestCase {
            input: "let fivePlusTen = fn() {5 + 10}; fivePlusTen();",
            expected: Object::Integer { value: 15 },
        },
        VmTestCase {
            input: "let one = fn() { 1; }; let two = fn() { 2; }; one() + two()",
            expected: Object::Integer { value: 3 },
        },
        VmTestCase {
            input: "let a = fn() { 1 }; let b = fn() { a() + 1 }; let c = fn() { b() + 1 }; c();",
            expected: Object::Integer { value: 3 },
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_functions_with_return_statement() {
    let tests = vec![
        VmTestCase {
            input: "let earlyExit = fn() { return 99; 100; }; earlyExit();",
            expected: Object::Integer { value: 99 },
        },
        VmTestCase {
            input: "let earlyExit = fn() { return 99; return 100; }; earlyExit();",
            expected: Object::Integer { value: 99 },
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_functions_without_return_value() {
    let tests = vec![
        VmTestCase {
            input: "let noReturn = fn() { }; noReturn();",
            expected: Object::Null,
        },
        VmTestCase {
            input: "let noReturn = fn() { }; let noReturnTwo = fn() { noReturn(); }; noReturn(); noReturnTwo();",
            expected: Object::Null,
        },
    ];
    run_vm_tests(tests);
}

#[test]
fn test_first_class_functions() {
    let tests = vec![VmTestCase {
        input: "let returnsOne = fn() { 1; }; let returnsOneReturner = fn() { returnsOne; }; returnsOneReturner()();",
        expected: Object::Integer { value: 1 },
    }];
    run_vm_tests(tests);
}

#[test]
fn test_calling_functions_with_bindings() {
    let tests = vec![
        VmTestCase {
            input: "let one = fn() { let one = 1; one }; one();",
            expected: Object::Integer { value: 1 },
        },
        VmTestCase {
            input: "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();",
            expected: Object::Integer { value: 3 },
        },
        VmTestCase {
            input: "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; let threeAndFour = fn() { let three = 3; let four = 4; three + four; }; oneAndTwo() + threeAndFour();",
            expected: Object::Integer { value: 10 },
        },
        VmTestCase {
            input: "let firstFoobar = fn() { let foobar = 50; foobar; }; let secondFoobar = fn() { let foobar = 100; foobar; }; firstFoobar() + secondFoobar();",
            expected: Object::Integer { value: 150 },
        },
        VmTestCase {
            input: "let globalSeed = 50; let minusOne = fn() { let num = 1; globalSeed - num; }; let minusTwo = fn() { let num = 2; globalSeed - num; }; minusOne() + minusTwo();",
            expected: Object::Integer { value: 97 },
        },
    ];
    run_vm_tests(tests);
}
