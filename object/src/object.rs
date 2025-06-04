use ast::{
    BlockStatement, Expression, IfExpression, InfixExpression, PrefixExpression, Program, Statement,
};

#[derive(Clone, Debug, PartialEq)]
pub enum ObjectType {
    Integer { value: i64 },
    Boolean { value: bool },
    Return { value: Box<ObjectType> },
    Error { message: String },
    Null,
}

pub const TRUE: ObjectType = ObjectType::Boolean { value: true };
pub const FALSE: ObjectType = ObjectType::Boolean { value: false };
pub const NULL: ObjectType = ObjectType::Null;

impl ObjectType {
    pub fn inspect(&self) -> String {
        match self {
            ObjectType::Null => "null".to_string(),
            ObjectType::Boolean { value } => value.to_string(),
            ObjectType::Integer { value } => value.to_string(),
            ObjectType::Return { .. } => "return_value".to_string(),
            ObjectType::Error { message } => format!("Error: {}", message),
        }
    }
    pub fn object_type(&self) -> String {
        match self {
            ObjectType::Null => "null".to_string(),
            ObjectType::Integer { .. } => "INTEGER".to_string(),
            ObjectType::Boolean { .. } => "BOOLEAN".to_string(),
            ObjectType::Return { .. } => "RETURN_VALUE".to_string(),
            ObjectType::Error { .. } => "ERROR".to_string(),
        }
    }
}

pub trait CoerceObject {
    fn coerce(&self) -> ObjectType;
}

fn is_err(obj: &ObjectType) -> bool {
    if let ObjectType::Error { .. } = obj {
        true
    } else {
        false
    }
}

impl CoerceObject for Statement {
    fn coerce(&self) -> ObjectType {
        match self {
            Statement::Expression { value, .. } => value.coerce(),
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    let coerced = expr.coerce();
                    if is_err(&coerced) {
                        coerced
                    } else {
                        ObjectType::Return {
                            value: Box::new(coerced),
                        }
                    }
                } else {
                    NULL
                }
            }
            _ => todo!("Not yet implemented"),
        }
    }
}

impl CoerceObject for Program {
    fn coerce(&self) -> ObjectType {
        let mut result: ObjectType = ObjectType::Null;
        for stmt in &self.statements {
            result = stmt.coerce();
            if let ObjectType::Return { value } = result {
                return *value;
            }
            if let ObjectType::Error { .. } = result {
                return result;
            }
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

fn eval_minus_prefix_operator_expression(right: ObjectType) -> ObjectType {
    if let ObjectType::Integer { value } = right {
        ObjectType::Integer { value: -value }
    } else {
        ObjectType::Error {
            message: format!("unkown operator: -{}", right.object_type()),
        }
    }
}

fn eval_prefix_expression(operator: &str, right: ObjectType) -> ObjectType {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => ObjectType::Error {
            message: format!("unkown operator: {}{}", operator, right.object_type()),
        },
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left: ObjectType,
    right: ObjectType,
) -> ObjectType {
    if let (
        &ObjectType::Integer { value: left_value },
        &ObjectType::Integer { value: right_value },
    ) = (&left, &right)
    {
        match operator {
            "+" => ObjectType::Integer {
                value: left_value + right_value,
            },
            "-" => ObjectType::Integer {
                value: left_value - right_value,
            },
            "*" => ObjectType::Integer {
                value: left_value * right_value,
            },
            "/" => ObjectType::Integer {
                value: left_value / right_value,
            },
            "<" => {
                if left_value < right_value {
                    TRUE
                } else {
                    FALSE
                }
            }
            ">" => {
                if left_value > right_value {
                    TRUE
                } else {
                    FALSE
                }
            }
            "==" => {
                if left_value == right_value {
                    TRUE
                } else {
                    FALSE
                }
            }
            "!=" => {
                if left_value != right_value {
                    TRUE
                } else {
                    FALSE
                }
            }
            _ => ObjectType::Error {
                message: format!(
                    "unkown operator: {} {} {}",
                    left.object_type(),
                    operator,
                    right.object_type()
                ),
            },
        }
    } else {
        panic!("Need two Integer arguments")
    }
}

fn eval_infix_expression(operator: &str, left: ObjectType, right: ObjectType) -> ObjectType {
    if let (&ObjectType::Integer { .. }, &ObjectType::Integer { .. }) = (&left, &right) {
        eval_integer_infix_expression(operator, left, right)
    } else if let (
        &ObjectType::Boolean { value: left_value },
        &ObjectType::Boolean { value: right_value },
    ) = (&left, &right)
    {
        match operator {
            "==" => {
                if left_value == right_value {
                    TRUE
                } else {
                    FALSE
                }
            }
            "!=" => {
                if left_value != right_value {
                    TRUE
                } else {
                    FALSE
                }
            }
            _ => ObjectType::Error {
                message: format!(
                    "unkown operator: {} {} {}",
                    left.object_type(),
                    operator,
                    right.object_type()
                ),
            },
        }
    } else {
        ObjectType::Error {
            message: format!(
                "type mismatch: {} {} {}",
                left.object_type(),
                operator,
                right.object_type()
            ),
        }
    }
}

impl CoerceObject for BlockStatement {
    fn coerce(&self) -> ObjectType {
        let mut result: ObjectType = ObjectType::Null;
        for stmt in &self.statements {
            result = stmt.coerce();
            if matches!(result, ObjectType::Return { .. } | ObjectType::Error { .. }) {
                return result;
            }
        }
        result
    }
}

fn is_truthy(expr: ObjectType) -> bool {
    match expr {
        ObjectType::Null => false,
        TRUE => true,
        FALSE => false,
        _ => true,
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
                if is_err(&evaluated_right) {
                    evaluated_right
                } else {
                    eval_prefix_expression(operator, evaluated_right)
                }
            }
            Expression::InfixExpression(InfixExpression {
                operator,
                left,
                right,
                ..
            }) => {
                let evaluated_left = left.coerce();
                if is_err(&evaluated_left) {
                    return evaluated_left;
                }
                let evaluated_right = right.coerce();
                if is_err(&evaluated_right) {
                    return evaluated_right;
                }
                eval_infix_expression(operator, evaluated_left, evaluated_right)
            }
            Expression::IfExpression(IfExpression {
                condition,
                consequence,
                alternative,
                ..
            }) => {
                let cond = condition.coerce();
                if is_err(&cond) {
                    return cond;
                }
                if is_truthy(cond) {
                    consequence.coerce()
                } else if let Some(alt) = alternative {
                    alt.coerce()
                } else {
                    NULL
                }
            }
            _ => todo!(),
        }
    }
}
