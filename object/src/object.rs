pub enum ObjectType {
    IntegerObj,
}

pub struct Integer {
    value: i64,
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
