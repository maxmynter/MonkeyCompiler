use crate::utils::prepare_program_for_test;
use compiler::Compiler;
use object::Object;
use vm::VM;

fn test_integer_object(expected: Object, actual: Object) -> Result<(), String> {
    assert_eq!(expected, actual);
    Ok(())
}

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
            &Object::Integer {
                value: expected_value,
            },
            &Object::Integer {
                value: actual_value,
            },
        ) => {
            let (expected_value, actual_value) = dbg!((expected_value, actual_value));
            assert_eq!(expected_value, actual_value);
        }
        _ => panic!("unexpected object value {:?}, got={:?}", actual, expected),
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
    ];

    run_vm_tests(tests);
}
