use code::Instruction;
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::{HashMap, hash_map::DefaultHasher};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use ast::{
    ArrayLiteral, BlockStatement, CallExpression, Expression, FunctionLiteral, HashLiteral,
    Identifier, IfExpression, IndexExpression, InfixExpression, Node, PrefixExpression, Program,
    Statement, StringLiteral,
};

type BuiltinFn = fn(args: Vec<Object>) -> Result<Object, EvalError>;

#[derive(PartialEq, Debug, Clone)]
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct HashKey {
    kind: String,
    value: u64,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}
impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn new_extended(outer: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: Some(outer.clone()),
        }))
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        if let Some(inner_value) = self.store.get(name) {
            Some(inner_value.clone())
        } else if let Some(outer) = &self.outer {
            outer.borrow().get(name)
        } else {
            None
        }
    }

    pub fn set(&mut self, name: &String, value: Object) {
        self.store.insert(name.to_string(), value);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Hash {
        pairs: HashMap<HashKey, HashPair>,
    },
    Integer {
        value: i64,
    },
    Boolean {
        value: bool,
    },
    Return {
        value: Box<Object>,
    },
    Function {
        parameters: Rc<Vec<Identifier>>,
        body: Rc<BlockStatement>,
        env: Rc<RefCell<Environment>>,
    },
    CompiledFunction {
        instructions: Instruction,
        num_locals: usize,
        num_parameters: usize,
    },
    String {
        value: String,
    },
    Builtin {
        func: BuiltinFn,
    },
    Array {
        elements: Vec<Object>,
    },
    Null,
}

pub trait ObjectTraits {
    fn inspect(&self) -> String;
    fn object_type(&self) -> String;
    fn hash(&self) -> Result<HashKey, EvalError>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum EvalError {
    Error { message: String },
    IndexError { message: String },
    Unhashable { message: String },
}

impl ObjectTraits for EvalError {
    fn inspect(&self) -> String {
        match &self {
            EvalError::Error { message } => format!("Error: {}", message),
            EvalError::IndexError { message } => format!("Index Error: {}", message),
            EvalError::Unhashable { message } => format!("Cannot hash unhashable: {}", message),
        }
    }

    fn object_type(&self) -> String {
        match &self {
            EvalError::Error { .. } => "ERROR".to_string(),
            EvalError::IndexError { .. } => "INDEX_ERROR".to_string(),
            EvalError::Unhashable { .. } => "UNHASHABLE_ERROR".to_string(),
        }
    }

    fn hash(&self) -> Result<HashKey, EvalError> {
        Err(EvalError::Unhashable {
            message: "cannot hash error".to_string(),
        })
    }
}

lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, BuiltinFn> = {
        [
            ("len", builtin_len as BuiltinFn),
            ("first", builtin_first as BuiltinFn),
            ("last", builtin_last as BuiltinFn),
            ("rest", builtin_rest as BuiltinFn),
            ("push", builtin_push as BuiltinFn),
            ("puts", builtin_puts as BuiltinFn),
        ]
        .iter()
        .cloned()
        .collect()
    };
}

fn expect_builtin_args_len<T>(args: &[T], length: usize) -> Result<(), EvalError> {
    if args.len() != length {
        Err(EvalError::Error {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        })
    } else {
        Ok(())
    }
}

fn builtin_puts(args: Vec<Object>) -> Result<Object, EvalError> {
    for arg in args {
        println!("{}", arg.inspect());
    }
    Ok(NULL)
}

fn builtin_push(args: Vec<Object>) -> Result<Object, EvalError> {
    expect_builtin_args_len(&args, 2)?;
    if let Object::Array { elements } = &args[0] {
        let mut extended_elements = elements.clone();
        extended_elements.push(args[1].clone());
        Ok(Object::Array {
            elements: extended_elements.to_vec(),
        })
    } else {
        Err(EvalError::Error {
            message: "can only apply push to type array".to_string(),
        })
    }
}

fn builtin_rest(args: Vec<Object>) -> Result<Object, EvalError> {
    expect_builtin_args_len(&args, 1)?;
    if let Object::Array { elements } = &args[0] {
        match elements.len() {
            0 => Ok(NULL),
            _ => Ok(Object::Array {
                elements: elements[1..].to_vec(),
            }),
        }
    } else {
        Err(EvalError::Error {
            message: "wrong argument type to `rest`, need array".to_string(),
        })
    }
}

fn builtin_first(args: Vec<Object>) -> Result<Object, EvalError> {
    expect_builtin_args_len(&args, 1)?;
    match &args[0] {
        Object::Array { elements } => {
            if !elements.is_empty() {
                Ok(elements[0].clone())
            } else {
                Ok(NULL)
            }
        }
        _ => Err(EvalError::Error {
            message: "`first` only works on arrays".to_string(),
        }),
    }
}

fn builtin_last(args: Vec<Object>) -> Result<Object, EvalError> {
    expect_builtin_args_len(&args, 1)?;
    match &args[0] {
        Object::Array { elements } => {
            if !elements.is_empty() {
                Ok(elements[elements.len() - 1].clone())
            } else {
                Ok(NULL)
            }
        }
        _ => Err(EvalError::Error {
            message: "`last` only works on arrays".to_string(),
        }),
    }
}

fn builtin_len(args: Vec<Object>) -> Result<Object, EvalError> {
    expect_builtin_args_len(&args, 1)?;
    match &args[0] {
        Object::String { value } => Ok(Object::Integer {
            value: value.len() as i64,
        }),
        Object::Array { elements } => Ok(Object::Integer {
            value: elements.len() as i64,
        }),
        _ => Err(EvalError::Error {
            message: format!(
                "argument to `len` not supported, got {}",
                args[0].object_type()
            ),
        }),
    }
}

pub const TRUE: Object = Object::Boolean { value: true };
pub const FALSE: Object = Object::Boolean { value: false };
pub const NULL: Object = Object::Null;

impl ObjectTraits for Object {
    fn inspect(&self) -> String {
        match self {
            Object::Null => "null".to_string(),
            Object::Boolean { value } => value.to_string(),
            Object::Integer { value } => value.to_string(),
            Object::String { value } => value.to_string(),
            Object::Return { .. } => "return_value".to_string(),
            Object::Builtin { .. } => "builtin_function".to_string(),
            Object::CompiledFunction { .. } => "compiled_function".to_string(),
            Object::Function {
                parameters, body, ..
            } => {
                let mut out = String::new();
                out.push_str("fn(");
                out.push_str(
                    &parameters
                        .iter()
                        .map(|p| p.as_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                out.push(')');
                out.push_str(&body.as_string());
                out
            }
            Object::Array { elements } => {
                let mut out = String::from("[");
                out.push_str(
                    &elements
                        .iter()
                        .map(|el| el.inspect())
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                out.push(']');
                out
            }
            Object::Hash { pairs } => {
                let mut out = String::from("{ ");
                out.push_str(
                    &pairs
                        .iter()
                        .map(|(_, pair)| {
                            format!("{}: {}", pair.key.inspect(), pair.value.inspect())
                        })
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                out.push('}');
                out
            }
        }
    }

    fn object_type(&self) -> String {
        match self {
            Object::Null => "null".to_string(),
            Object::Integer { .. } => "INTEGER".to_string(),
            Object::Boolean { .. } => "BOOLEAN".to_string(),
            Object::Return { .. } => "RETURN_VALUE".to_string(),
            Object::Builtin { .. } => "BUILTIN_FUNCTION".to_string(),
            Object::Function { .. } => "FUNCTION".to_string(),
            Object::String { .. } => "STRING".to_string(),
            Object::Array { .. } => "ARRAY".to_string(),
            Object::Hash { .. } => "HASH".to_string(),
            Object::CompiledFunction { .. } => "COMPILED_FUNCTION".to_string(),
        }
    }

    fn hash(&self) -> Result<HashKey, EvalError> {
        match &self {
            Object::String { value } => {
                let mut hasher = DefaultHasher::new();
                value.hash(&mut hasher);
                Ok(HashKey {
                    kind: self.object_type(),
                    value: hasher.finish(),
                })
            }
            Object::Boolean { value } => Ok(HashKey {
                value: if *value { 1 } else { 0 },
                kind: self.object_type(),
            }),
            Object::Integer { value } => Ok(HashKey {
                kind: self.object_type(),
                value: *value as u64,
            }),
            other => Err(EvalError::Unhashable {
                message: format!("cannot hash: {}", other.object_type()),
            }),
        }
    }
}

pub trait CoerceObject {
    type Coerced;
    fn evaluate(&self, environment: &Rc<RefCell<Environment>>) -> Result<Self::Coerced, EvalError>;
}

impl CoerceObject for Vec<Expression> {
    type Coerced = Vec<Object>;
    fn evaluate(&self, environment: &Rc<RefCell<Environment>>) -> Result<Vec<Object>, EvalError> {
        self.iter()
            .map(|ob| ob.evaluate(environment))
            .collect::<Result<Vec<Object>, EvalError>>()
    }
}

impl CoerceObject for Statement {
    type Coerced = Object;
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Self::Coerced, EvalError> {
        match self {
            Statement::Expression { value, .. } => value.evaluate(env),
            Statement::Return { value, .. } => {
                let coerced = value.evaluate(env)?;
                Ok(Object::Return {
                    value: Box::new(coerced),
                })
            }
            Statement::Let { name, value, .. } => {
                let binding = value.evaluate(env)?;
                env.borrow_mut().set(&name.value, binding.clone());
                Ok(binding)
            }
        }
    }
}

impl CoerceObject for Program {
    type Coerced = Object;
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Self::Coerced, EvalError> {
        let mut result: Object = Object::Null;
        for stmt in &self.statements {
            result = stmt.evaluate(env)?;
            if let Object::Return { value } = result {
                return Ok(*value);
            }
        }
        Ok(result)
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

fn eval_minus_prefix_operator_expression(right: Object) -> Result<Object, EvalError> {
    if let Object::Integer { value } = right {
        Ok(Object::Integer { value: -value })
    } else {
        Err(EvalError::Error {
            message: format!("unkown operator: -{}", right.object_type()),
        })
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Result<Object, EvalError> {
    let result = match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right)?,
        _ => {
            return Err(EvalError::Error {
                message: format!("unkown operator: {}{}", operator, right.object_type()),
            });
        }
    };
    Ok(result)
}

fn eval_integer_infix_expression(
    operator: &str,
    left: Object,
    right: Object,
) -> Result<Object, EvalError> {
    if let (&Object::Integer { value: left_value }, &Object::Integer { value: right_value }) =
        (&left, &right)
    {
        let result = match operator {
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
            _ => {
                return Err(EvalError::Error {
                    message: format!(
                        "unkown operator: {} {} {}",
                        left.object_type(),
                        operator,
                        right.object_type()
                    ),
                });
            }
        };
        Ok(result)
    } else {
        panic!("Need two Integer arguments")
    }
}

fn eval_boolean_infix_expression(
    operator: &str,
    left: Object,
    right: Object,
) -> Result<Object, EvalError> {
    match operator {
        "==" => {
            if left == right {
                Ok(TRUE)
            } else {
                Ok(FALSE)
            }
        }
        "!=" => {
            if left != right {
                Ok(TRUE)
            } else {
                Ok(FALSE)
            }
        }
        _ => Err(EvalError::Error {
            message: format!(
                "unkown operator: {} {} {}",
                left.object_type(),
                operator,
                right.object_type()
            ),
        }),
    }
}

fn eval_string_infix_expression(
    operator: &str,
    left_str: &String,
    right_str: &String,
) -> Result<Object, EvalError> {
    match operator {
        "+" => Ok(Object::String {
            value: format!("{}{}", left_str, right_str),
        }),
        _ => Err(EvalError::Error {
            message: format!("unknown operator: STRING {} STRING", operator),
        }),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Result<Object, EvalError> {
    let result = match (&left, &right) {
        (&Object::Integer { .. }, &Object::Integer { .. }) => {
            eval_integer_infix_expression(operator, left, right)?
        }
        (&Object::Boolean { .. }, &Object::Boolean { .. }) => {
            eval_boolean_infix_expression(operator, left, right)?
        }
        (Object::String { value: left_value }, Object::String { value: right_value }) => {
            eval_string_infix_expression(operator, left_value, right_value)?
        }
        _ => {
            return Err(EvalError::Error {
                message: format!(
                    "type mismatch: {} {} {}",
                    left.object_type(),
                    operator,
                    right.object_type()
                ),
            });
        }
    };
    Ok(result)
}

impl CoerceObject for BlockStatement {
    type Coerced = Object;
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Self::Coerced, EvalError> {
        let mut result: Object = Object::Null;
        for stmt in &self.statements {
            result = stmt.evaluate(env)?;
            if let Object::Return { .. } = result {
                return Ok(result);
            }
        }
        Ok(result)
    }
}

pub fn is_truthy(expr: Object) -> bool {
    match expr {
        NULL => false,
        TRUE => true,
        FALSE => false,
        _ => true,
    }
}

impl CoerceObject for Expression {
    type Coerced = Object;
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Self::Coerced, EvalError> {
        let result = match self {
            Expression::IntegerLiteral(int) => Object::Integer { value: int.value },
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
                let evaluated_right = right.evaluate(env)?;
                eval_prefix_expression(operator, evaluated_right)?
            }
            Expression::InfixExpression(InfixExpression {
                operator,
                left,
                right,
                ..
            }) => {
                let evaluated_left = left.evaluate(env)?;
                let evaluated_right = right.evaluate(env)?;
                eval_infix_expression(operator, evaluated_left, evaluated_right)?
            }
            Expression::IfExpression(IfExpression {
                condition,
                consequence,
                alternative,
                ..
            }) => {
                let cond = condition.evaluate(env)?;
                if is_truthy(cond) {
                    consequence.evaluate(env)?
                } else if let Some(alt) = alternative {
                    alt.evaluate(env)?
                } else {
                    NULL
                }
            }
            Expression::Identifier(Identifier { value, .. }) => {
                if let Some(inner_value) = env.borrow_mut().get(value) {
                    inner_value.clone()
                } else if let Some(builtin) = BUILTINS.get(value.as_str()) {
                    Object::Builtin { func: *builtin }
                } else {
                    return Err(EvalError::Error {
                        message: format!("identifier not found: {}", value),
                    });
                }
            }
            Expression::FunctionLiteral(FunctionLiteral {
                parameters, body, ..
            }) => Object::Function {
                parameters: parameters.clone(),
                body: body.clone(),
                env: env.clone(),
            },
            Expression::CallExpression(CallExpression {
                function,
                arguments,
                ..
            }) => {
                let func = function.evaluate(env)?;
                let args = arguments.evaluate(env)?;
                apply_function(func, args)?
            }
            Expression::String(StringLiteral { value, .. }) => Object::String {
                value: value.to_string(),
            },
            Expression::Array(ArrayLiteral { elements, .. }) => {
                let elements = elements.evaluate(env)?;
                Object::Array { elements }
            }
            Expression::Index(IndexExpression { left, index, .. }) => {
                let left = left.evaluate(env)?;
                let index = index.evaluate(env)?;
                evaluate_index_expression(left, index)?
            }
            Expression::HashMap(HashLiteral { pairs, .. }) => {
                let mut hash_pairs = HashMap::new();
                for (key_expr, val_expr) in pairs {
                    let key = key_expr.evaluate(env)?;
                    let value = val_expr.evaluate(env)?;
                    let hash_key = key.hash()?;
                    hash_pairs.insert(hash_key, HashPair { key, value });
                }
                Object::Hash { pairs: hash_pairs }
            }
        };
        Ok(result)
    }
}

fn evaluate_index_expression(left: Object, index: Object) -> Result<Object, EvalError> {
    match (&left, &index) {
        (Object::Array { elements }, Object::Integer { value }) => {
            eval_array_index_expression(elements, *value)
        }
        (Object::Hash { .. }, _) => eval_hash_index_expression(left, index),
        _ => Err(EvalError::Error {
            message: format!("index operator not supported: {}", left.object_type()),
        }),
    }
}

fn eval_hash_index_expression(left: Object, index: Object) -> Result<Object, EvalError> {
    if let Ok(hash) = index.hash() {
        if let Object::Hash { pairs } = left {
            match pairs.get(&hash) {
                Some(pair) => Ok(pair.value.clone()),
                None => Ok(NULL),
            }
        } else {
            panic!("Expected left Hash Object");
        }
    } else {
        Err(EvalError::Unhashable {
            message: format!("unusable as hash key: {}", index.object_type()),
        })
    }
}

fn eval_array_index_expression(arr: &[Object], idx: i64) -> Result<Object, EvalError> {
    if idx < 0 {
        return Err(EvalError::IndexError {
            message: "index must not be negative".to_string(),
        });
    }
    match arr.get(idx as usize) {
        Some(val) => Ok(val.clone()),
        None => Err(EvalError::IndexError {
            message: format!(
                "index, {}, out of bounds for array of length {}",
                idx,
                arr.len()
            ),
        }),
    }
}

fn apply_function(func: Object, args: Vec<Object>) -> Result<Object, EvalError> {
    match func {
        Object::Function {
            parameters,
            body,
            env,
        } => {
            let extended_env = extend_environment(env.clone(), args, parameters);
            let returned = body.evaluate(&extended_env)?;
            if let Object::Return { value } = returned {
                Ok(*value)
            } else {
                Ok(returned)
            }
        }
        Object::Builtin {
            func: builtin_function,
        } => builtin_function(args),
        _ => Err(EvalError::Error {
            message: format!("not a function: {}", func.inspect()),
        }),
    }
}

fn extend_environment(
    env: Rc<RefCell<Environment>>,
    args: Vec<Object>,
    params: Rc<Vec<Identifier>>,
) -> Rc<RefCell<Environment>> {
    let extended_env = Environment::new_extended(env.clone());
    for (idx, param) in params.iter().enumerate() {
        extended_env
            .borrow_mut()
            .set(&param.value, args[idx].clone());
    }
    extended_env
}
