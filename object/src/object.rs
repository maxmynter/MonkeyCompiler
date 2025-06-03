use ast::{Boolean, Expression, InfixExpression, PrefixExpression, Program, Statement};

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

fn eval_minus_prefix_operator_expression(right: ObjectType) -> ObjectType {
    if let ObjectType::Integer { value } = right {
        ObjectType::Integer { value: -value }
    } else {
        NULL
    }
}

fn eval_prefix_expression(operator: &str, right: ObjectType) -> ObjectType {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => NULL,
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left: ObjectType,
    right: ObjectType,
) -> ObjectType {
    if let (ObjectType::Integer { value: left_value }, ObjectType::Integer { value: right_value }) =
        (left, right)
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
            _ => NULL,
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
            _ => NULL,
        }
    } else {
        NULL
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
            Expression::InfixExpression(InfixExpression {
                operator,
                left,
                right,
                ..
            }) => {
                let evaluated_left = left.coerce();
                let evaluated_right = right.coerce();
                eval_infix_expression(operator, evaluated_left, evaluated_right)
            }
            _ => todo!(),
        }
    }
}
