use object::{CoerceObject, Environment, Object};
use std::cell::RefCell;
use std::rc::Rc;

pub fn eval(program: impl CoerceObject, environment: Rc<RefCell<Environment>>) -> Object {
    program.coerce(environment.clone()).unwrap()
}
