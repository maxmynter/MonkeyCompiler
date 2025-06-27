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
        let _ = comp.compile(program).unwrap();
        let vm = VM::new(comp.bytecode());
        let stackElem = vm.stack_top().unwrap().clone();
        test_expected_object(tt.expected, stackElem);
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
        ) => assert_eq!(expected_value, actual_value),
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
            expected: Object::Integer { value: 2 }, //TODO: FIXME
        },
    ];

    run_vm_tests(tests);
}
