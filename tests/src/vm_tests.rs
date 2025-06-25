use crate::utils::prepare_program_for_test;
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
