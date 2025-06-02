use object::{CoerceObject, ObjectType};

pub fn eval(program: impl CoerceObject) -> ObjectType {
    program.coerce()
}
