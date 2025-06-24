use core::fmt;
use lexer::{Token, TokenType};
use std::rc::Rc;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: Vec<(Expression, Expression)>, // Use vec here to stay hashable
}

impl Node for HashLiteral {
    fn as_string(&self) -> String {
        let mut out = String::from("{ ");
        out.push_str(
            &self
                .pairs
                .iter()
                .map(|(key, value)| format!("{}:{}", key.as_string(), value.as_string()))
                .collect::<Vec<String>>()
                .join(", "),
        );
        out.push_str(" }");
        out
    }
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl Node for IndexExpression {
    fn as_string(&self) -> String {
        let mut out = String::from("(");
        out.push_str(&self.left.to_string());
        out.push('[');
        out.push_str(&self.index.to_string());
        out.push_str("])");
        out
    }
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl Node for ArrayLiteral {
    fn as_string(&self) -> String {
        let mut out = String::from("[");
        out.push_str(
            &self
                .elements
                .iter()
                .map(|el| el.as_string())
                .collect::<Vec<_>>()
                .join(", "),
        );
        out.push(']');
        out
    }

    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }
}

#[derive(PartialEq, Clone, Debug, Hash, Eq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }

    fn as_string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.function.to_string());
        out.push('(');
        out.push_str(
            &self
                .arguments
                .iter()
                .map(|f| f.as_string())
                .collect::<Vec<_>>()
                .join(", "),
        );
        out.push(')');
        out
    }
}

#[derive(PartialEq, Clone, Debug, Hash, Eq)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Node for StringLiteral {
    fn as_string(&self) -> String {
        self.value.clone()
    }

    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }
}

#[derive(PartialEq, Clone, Debug, Hash, Eq)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }

    fn as_string(&self) -> String {
        self.token.literal.to_string()
    }
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct Identifier {
    pub token: Token,
    pub value: Rc<String>,
}

impl Identifier {
    pub fn as_expression(&self) -> Expression {
        Expression::Identifier(Identifier {
            token: self.token.clone(),
            value: self.value.clone(),
        })
    }
    pub fn as_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(PartialEq, Clone, Debug, Hash, Eq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }
    fn as_string(&self) -> String {
        self.token.literal.to_string()
    }
}

#[derive(PartialEq, Clone, Debug, Hash, Eq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }

    fn as_string(&self) -> String {
        let mut out = String::new();
        out.push('(');
        out.push_str(&self.operator);
        out.push_str(&self.right.as_string());
        out.push(')');
        out
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct InfixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
    pub left: Box<Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }
    fn as_string(&self) -> String {
        let mut out = String::new();
        out.push('(');
        out.push_str(&self.left.as_string());
        out.push(' ');
        out.push_str(&self.operator);
        out.push(' ');
        out.push_str(&self.right.as_string());
        out.push(')');
        out
    }
}

#[derive(PartialEq, Clone, Debug, Hash, Eq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Rc<Vec<Identifier>>,
    pub body: Rc<BlockStatement>,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }

    fn as_string(&self) -> String {
        let mut out = String::new();
        out.push_str("fn(");
        out.push_str(
            &self
                .parameters
                .iter()
                .map(|f| f.as_string())
                .collect::<Vec<_>>()
                .join(", "),
        );
        out.push(')');
        out.push_str(&self.body.as_string());
        out
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Boolean(Boolean),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    String(StringLiteral),
    Array(ArrayLiteral),
    Index(IndexExpression),
    HashMap(HashLiteral),
}

impl Expression {
    pub fn from_int(int: i64) -> Expression {
        Self::IntegerLiteral(IntegerLiteral {
            token: Token {
                kind: TokenType::INT,
                literal: int.to_string().into(),
            },
            value: int,
        })
    }

    pub fn from_bool(boolean: bool) -> Expression {
        Self::Boolean(Boolean {
            token: Token {
                kind: if boolean {
                    TokenType::TRUE
                } else {
                    TokenType::FALSE
                },
                literal: boolean.to_string().into(),
            },
            value: boolean,
        })
    }

    pub fn from_string(string: String) -> Expression {
        Self::String(StringLiteral {
            token: Token {
                kind: TokenType::STRING,
                literal: string.clone().into(),
            },
            value: string,
        })
    }
}

#[derive(PartialEq, Clone, Debug, Hash, Eq)]
pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        value: Expression,
    },
    Return {
        token: Token,
        value: Expression,
    },
    Expression {
        token: Token,
        value: Expression,
    },
}

pub trait Node {
    fn token_literal(&self) -> Rc<String>;
    fn as_string(&self) -> String;
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn as_string(&self) -> String {
        let mut out = String::from("{\n");
        out.push_str(
            &self
                .statements
                .iter()
                .map(|s| format!(" {}", s.as_string()))
                .collect::<Vec<_>>()
                .join("\n"),
        );
        out.push_str("\n}");
        out
    }

    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }

    fn as_string(&self) -> String {
        let mut out = String::new();
        out.push_str("if ");
        out.push_str(&self.condition.as_string());
        out.push_str(&self.consequence.as_string());
        if let Some(alternative) = &self.alternative {
            out.push_str(" else ");
            out.push_str(&alternative.as_string());
        }
        out
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> Rc<String> {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            Rc::new(String::new())
        }
    }

    fn as_string(&self) -> String {
        let mut out = String::new();
        for stmt in &self.statements {
            out.push_str(&stmt.as_string())
        }
        out
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt.as_string())?;
        }
        Ok(())
    }
}

impl Node for Statement {
    fn token_literal(&self) -> Rc<String> {
        match self {
            Statement::Let { token, .. }
            | Statement::Return { token, .. }
            | Statement::Expression { token, .. } => token.literal.clone(),
        }
    }

    fn as_string(&self) -> String {
        let mut out = String::new();
        match self {
            Statement::Let { token, name, value } => {
                out.push_str(&token.literal);
                out.push(' ');
                out.push_str(&name.value);
                out.push_str(" = ");
                out.push_str(&value.as_string());
                out.push(';');
            }
            Statement::Return { token, value } => {
                out.push_str(&token.literal);
                out.push(' ');
                out.push_str(&value.as_string());
                out.push(';');
            }
            Statement::Expression { value, .. } => {
                out.push_str(&value.as_string());
            }
        }
        out
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

impl Node for Expression {
    fn token_literal(&self) -> Rc<String> {
        match self {
            Expression::Identifier(Identifier { token, .. })
            | Expression::HashMap(HashLiteral { token, .. })
            | Expression::Index(IndexExpression { token, .. })
            | Expression::String(StringLiteral { token, .. })
            | Expression::CallExpression(CallExpression { token, .. })
            | Expression::Boolean(Boolean { token, .. })
            | Expression::IntegerLiteral(IntegerLiteral { token, .. })
            | Expression::InfixExpression(InfixExpression { token, .. })
            | Expression::PrefixExpression(PrefixExpression { token, .. })
            | Expression::IfExpression(IfExpression { token, .. })
            | Expression::Array(ArrayLiteral { token, .. })
            | Expression::FunctionLiteral(FunctionLiteral { token, .. }) => token.literal.clone(),
        }
    }

    fn as_string(&self) -> String {
        match self {
            Expression::String(StringLiteral { value, .. }) => value.to_string(),
            Expression::Identifier(Identifier { value, .. }) => value.to_string(),
            Expression::Index(index) => index.as_string(),
            Expression::IntegerLiteral(IntegerLiteral { value, .. }) => value.to_string(),
            Expression::HashMap(hash) => hash.as_string(),
            Expression::PrefixExpression(prefix) => prefix.as_string(),
            Expression::InfixExpression(infix) => infix.as_string(),
            Expression::FunctionLiteral(fn_lit) => fn_lit.as_string(),
            Expression::Boolean(boolean) => boolean.as_string(),
            Expression::IfExpression(ifex) => ifex.as_string(),
            Expression::CallExpression(call) => call.as_string(),
            Expression::Array(arr) => arr.as_string(),
        }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }
    fn as_string(&self) -> String {
        self.value.to_string()
    }
}
