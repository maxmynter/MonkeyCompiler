use std::collections::HashMap;

use ast::{
    BlockStatement, Expression, Identifier, IfExpression, InfixExpression, PrefixExpression,
    Program, Statement,
};

pub struct Environment {
    store: HashMap<String, Object>,
}
impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &String) -> Option<&Object> {
        self.store.get(name)
    }

    pub fn set(&mut self, name: &String, value: Object) {
        self.store.insert(name.to_string(), value);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer { value: i64 },
    Boolean { value: bool },
    Return { value: Box<Object> },
    Error { message: String },
    Null,
}

pub const TRUE: Object = Object::Boolean { value: true };
pub const FALSE: Object = Object::Boolean { value: false };
pub const NULL: Object = Object::Null;

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Null => "null".to_string(),
            Object::Boolean { value } => value.to_string(),
            Object::Integer { value } => value.to_string(),
            Object::Return { .. } => "return_value".to_string(),
            Object::Error { message } => format!("Error: {}", message),
        }
    }
    pub fn object_type(&self) -> String {
        match self {
            Object::Null => "null".to_string(),
            Object::Integer { .. } => "INTEGER".to_string(),
            Object::Boolean { .. } => "BOOLEAN".to_string(),
            Object::Return { .. } => "RETURN_VALUE".to_string(),
            Object::Error { .. } => "ERROR".to_string(),
        }
    }
}

pub trait CoerceObject {
    fn coerce(&self, environment: &mut Environment) -> Object;
}

fn is_err(obj: &Object) -> bool {
    if let Object::Error { .. } = obj {
        true
    } else {
        false
    }
}

impl CoerceObject for Statement {
    fn coerce(&self, env: &mut Environment) -> Object {
        match self {
            Statement::Expression { value, .. } => value.coerce(env),
            Statement::Return { value, .. } => {
                let coerced = value.coerce(env);
                if is_err(&coerced) {
                    coerced
                } else {
                    Object::Return {
                        value: Box::new(coerced),
                    }
                }
            }
            Statement::Let { token, name, value } => {
                let binding = value.coerce(env);
                if is_err(&binding) {
                    return binding;
                }
                env.set(&name.value, binding.clone());
                binding
            }
        }
    }
}

impl CoerceObject for Program {
    fn coerce(&self, env: &mut Environment) -> Object {
        let mut result: Object = Object::Null;
        for stmt in &self.statements {
            result = stmt.coerce(env);
            if let Object::Return { value } = result {
                return *value;
            }
            if let Object::Error { .. } = result {
                return result;
            }
        }
        result
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean { value } if value => FALSE,
        Object::Boolean { value } if !value => TRUE,
        Object::Null => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    if let Object::Integer { value } = right {
        Object::Integer { value: -value }
    } else {
        Object::Error {
            message: format!("unkown operator: -{}", right.object_type()),
        }
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Error {
            message: format!("unkown operator: {}{}", operator, right.object_type()),
        },
    }
}

fn eval_integer_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    if let (&Object::Integer { value: left_value }, &Object::Integer { value: right_value }) =
        (&left, &right)
    {
        match operator {
            "+" => Object::Integer {
                value: left_value + right_value,
            },
            "-" => Object::Integer {
                value: left_value - right_value,
            },
            "*" => Object::Integer {
                value: left_value * right_value,
            },
            "/" => Object::Integer {
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
            _ => Object::Error {
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

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    if let (&Object::Integer { .. }, &Object::Integer { .. }) = (&left, &right) {
        eval_integer_infix_expression(operator, left, right)
    } else if let (
        &Object::Boolean { value: left_value },
        &Object::Boolean { value: right_value },
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
            _ => Object::Error {
                message: format!(
                    "unkown operator: {} {} {}",
                    left.object_type(),
                    operator,
                    right.object_type()
                ),
            },
        }
    } else {
        Object::Error {
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
    fn coerce(&self, env: &mut Environment) -> Object {
        let mut result: Object = Object::Null;
        for stmt in &self.statements {
            result = stmt.coerce(env);
            if matches!(result, Object::Return { .. } | Object::Error { .. }) {
                return result;
            }
        }
        result
    }
}

fn is_truthy(expr: Object) -> bool {
    match expr {
        Object::Null => false,
        TRUE => true,
        FALSE => false,
        _ => true,
    }
}

impl CoerceObject for Expression {
    fn coerce(&self, env: &mut Environment) -> Object {
        match self {
            Expression::IntegerLiteral(int) => Object::Integer { value: *int.value },
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
                let evaluated_right = right.coerce(env);
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
                let evaluated_left = left.coerce(env);
                if is_err(&evaluated_left) {
                    return evaluated_left;
                }
                let evaluated_right = right.coerce(env);
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
                let cond = condition.coerce(env);
                if is_err(&cond) {
                    return cond;
                }
                if is_truthy(cond) {
                    consequence.coerce(env)
                } else if let Some(alt) = alternative {
                    alt.coerce(env)
                } else {
                    NULL
                }
            }
            Expression::Identifier(Identifier { value, .. }) => {
                if let Some(inner_value) = env.get(&value) {
                    inner_value.clone()
                } else {
                    Object::Error {
                        message: format!("identifier not found: {}", value),
                    }
                }
            }
            _ => {
                todo!()
            }
        }
    }
}
