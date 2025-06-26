use crate::utils::prepare_program_for_test;
use compiler::Compiler;
use object::Object;

fn test_integer_object(expected: i64, actual: Object) -> Result<(), String> {
    match actual {
        Object::Integer { value } => {
            assert_eq!(value, expected);
            Ok(())
        }
        _ => Err("Not an integer object".to_string()),
    }
}

struct VmTestCase<T> {
    input: &'static str,
    expected: T,
}

fn run_vm_tests<T>(tests: Vec<VmTestCase<T>>) {
    for tt in tests {
        let program = prepare_program_for_test(tt.input)
        let mut comp = Compiler::new();
        comp.compile(program);
    }
}
