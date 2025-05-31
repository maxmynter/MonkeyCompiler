pub enum ObjectType {
    IntegerObj,
    BooleanObj,
}

pub struct Integer {
    value: i64,
}

pub struct Boolean {
    value: bool,
}

trait Inspect {
    fn inspect(&self) -> String;
}

trait TypedObject {
    fn object_type(&self) -> ObjectType;
}

impl Inspect for Integer {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

impl TypedObject for Integer {
    fn object_type(&self) -> ObjectType {
        ObjectType::IntegerObj
    }
}

impl Inspect for Boolean {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

impl TypedObject for Boolean {
    fn object_type(&self) -> ObjectType {
        ObjectType::BooleanObj
    }
}
