pub enum ObjectType {
    IntegerObj,
    BooleanObj,
    NullObj,
}

pub struct Integer {
    value: i64,
}

pub struct Boolean {
    value: bool,
}

pub struct Null {}

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

impl Inspect for Null {
    fn inspect(&self) -> String {
        "null".to_string()
    }
}

impl TypedObject for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::NullObj
    }
}
