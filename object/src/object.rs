use ast::{Expression, Program, Statement};

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

pub trait CoerceObject {
    fn coerce(&self) -> ObjectType;
}

impl CoerceObject for Statement {
    fn coerce(&self) -> ObjectType {
        match self {
            Statement::Expression { value, .. } => value.coerce(),
            _ => todo!(),
        }
    }
}

impl CoerceObject for Program {
    fn coerce(&self) -> ObjectType {
        let mut result: ObjectType = ObjectType::Null;
        for stmt in &self.statements {
            result = stmt.coerce()
        }
        result
    }
}

impl CoerceObject for Expression {
    fn coerce(&self) -> ObjectType {
        match self {
            Expression::IntegerLiteral(int) => ObjectType::Integer { value: *int.value },
            Expression::Boolean(boolean) => ObjectType::Boolean {
                value: boolean.value,
            },
            _ => todo!(),
        }
    }
}
