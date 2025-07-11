#![allow(dead_code)]
use crate::utils::prepare_program_for_test;
use compiler::Compiler;
use object::Object;
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
