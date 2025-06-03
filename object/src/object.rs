use ast::{Boolean, Expression, PrefixExpression, Program, Statement};

pub enum ObjectType {
    Integer { value: i64 },
    Boolean { value: bool },
    Null,
}

const TRUE: ObjectType = ObjectType::Boolean { value: true };
const FALSE: ObjectType = ObjectType::Boolean { value: false };
const NULL: ObjectType = ObjectType::Null;

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

fn eval_bang_operator_expression(right: ObjectType) -> ObjectType {
    match right {
        ObjectType::Boolean { value } if value => FALSE,
        ObjectType::Boolean { value } if !value => TRUE,
        ObjectType::Null => TRUE,
        _ => FALSE,
    }
}

fn eval_prefix_expression(operator: &str, right: ObjectType) -> ObjectType {
    match operator {
        "!" => eval_bang_operator_expression(right),
        _ => NULL,
    }
}
impl CoerceObject for Expression {
    fn coerce(&self) -> ObjectType {
        match self {
            Expression::IntegerLiteral(int) => ObjectType::Integer { value: *int.value },
            Expression::Boolean(boolean) => {
                if boolean.value {
                    TRUE
                } else {
                    FALSE
                }
            }
            Expression::PrefixExpression(PrefixExpression {
                right, operator, ..
            }) => {
                let evaluated_right = right.coerce();
                eval_prefix_expression(operator, evaluated_right)
            }
            _ => todo!(),
        }
    }
}
