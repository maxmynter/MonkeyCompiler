use object::{CoerceObject, Environment, Object};

pub fn eval(program: impl CoerceObject, environment: &mut Environment) -> Object {
    program.coerce(environment)
}
