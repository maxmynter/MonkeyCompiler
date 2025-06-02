use ast::Expression;

pub enum ObjectType {
    Integer { value: i64 },
    Boolean { value: bool },
    Null,
}

impl ObjectType {
    pub fn inspect(&self) -> String {
        match self {
            ObjectType::Null => "null".to_string(),
            ObjectType::Boolean { value } => value.to_string(),
            ObjectType::Integer { value } => value.to_string(),
        }
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
