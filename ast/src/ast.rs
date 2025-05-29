use core::fmt;
use lexer::{Token, TokenType};
use std::rc::Rc;

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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
}

#[derive(Clone, Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: Rc<i64>,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }
    fn as_string(&self) -> String {
        self.token.literal.to_string()
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> Rc<String> {
        self.token.literal.clone()
    }

    fn as_string(&self) -> String {
        let mut out = String::new();
        out.push_str("(");
        out.push_str(
            &self
                .parameters
                .iter()
                .map(|f| f.as_string())
                .collect::<Vec<_>>()
                .join(", "),
        );
        out.push_str(")");
        out
    }
}

#[derive(Clone, Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Boolean(Boolean),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
}

impl Expression {
    pub fn from_int(int: i64) -> Expression {
        Self::IntegerLiteral(IntegerLiteral {
            token: Token {
                kind: TokenType::INT,
                literal: int.to_string().into(),
            },
            value: int.into(),
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
}

#[derive(Clone, Debug)]
pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        value: Option<Expression>,
    },
    Return {
        token: Token,
        value: Option<Expression>,
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

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn as_string(&self) -> String {
        let mut out = String::new();
        for s in &self.statements {
            out.push_str(&s.to_string())
        }
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

#[derive(Debug, Clone)]
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
        out.push_str("if");
        out.push_str(&self.condition.as_string());
        out.push_str(&self.consequence.as_string());
        if let Some(alternative) = &self.alternative {
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
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

impl Node for Statement {
    fn token_literal(&self) -> Rc<String> {
        match self {
            Statement::Let { token, .. } => token.literal.clone(),
            Statement::Return { token, .. } => token.literal.clone(),
            Statement::Expression { token, .. } => token.literal.clone(),
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
                if let Some(expr) = value {
                    out.push_str(&expr.as_string());
                }
                out.push(';');
            }
            Statement::Return { token, value } => {
                out.push_str(&token.literal);
                out.push(' ');
                if let Some(ret_val) = value {
                    out.push_str(&ret_val.as_string());
                }
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
        match self {
            Statement::Let { name, value, .. } => {
                write!(f, "let {} = ", name.value)?;
                if let Some(val) = value {
                    write!(f, "{}", val)?;
                }
                write!(f, ";")
            }
            Statement::Return { value, .. } => {
                write!(f, "return ")?;
                if let Some(val) = value {
                    write!(f, "{}", val)?;
                }
                write!(f, ";")
            }
            Statement::Expression { value, .. } => {
                write!(f, "{}", value)
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident.value),
            Expression::Boolean(boolean) => write!(f, "{}", boolean.value),
            Expression::IntegerLiteral(int_lit) => write!(f, "{}", int_lit.value),
            Expression::PrefixExpression(prefix) => {
                write!(f, "({}{})", prefix.operator, prefix.right)
            }
            Expression::InfixExpression(infix) => {
                write!(f, "({} {} {})", infix.left, infix.operator, infix.right)
            }
            Expression::IfExpression(ifex) => {
                if let Some(alternative) = &ifex.alternative {
                    write!(
                        f,
                        "(if ({}) {{{}}} else {{{}}})",
                        ifex.condition, ifex.consequence, alternative
                    )
                } else {
                    write!(f, "(if ({}) {{{}}})", ifex.condition, ifex.consequence)
                }
            }
            Expression::FunctionLiteral(fn_lit) => {
                let params = fn_lit
                    .parameters
                    .iter()
                    .map(|f| f.as_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fn ({}) {{{}}}", params, fn_lit.body)
            }
        }
    }
}

impl Node for Expression {
    fn token_literal(&self) -> Rc<String> {
        match self {
            Expression::Identifier(ident) => ident.token_literal(),
            Expression::Boolean(Boolean { token, .. })
            | Expression::IntegerLiteral(IntegerLiteral { token, .. })
            | Expression::InfixExpression(InfixExpression { token, .. })
            | Expression::PrefixExpression(PrefixExpression { token, .. })
            | Expression::IfExpression(IfExpression { token, .. })
            | Expression::FunctionLiteral(FunctionLiteral { token, .. }) => token.literal.clone(),
        }
    }

    fn as_string(&self) -> String {
        match self {
            Expression::Identifier(Identifier { value, .. }) => value.to_string(),
            Expression::IntegerLiteral(IntegerLiteral { value, .. }) => value.to_string(),
            Expression::PrefixExpression(prefix) => prefix.as_string(),
            Expression::InfixExpression(infix) => infix.as_string(),
            Expression::FunctionLiteral(fn_lit) => fn_lit.as_string(),
            Expression::Boolean(boolean) => boolean.as_string(),
            Expression::IfExpression(ifex) => ifex.as_string(),
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
#[test]
fn test_ast() {
    let prog = Program {
        statements: vec![Statement::Let {
            token: Token {
                kind: TokenType::LET,
                literal: "let".to_string().into(),
            },
            name: Identifier {
                token: Token {
                    kind: TokenType::IDENT,
                    literal: "myVar".to_string().into(),
                },
                value: "myVar".to_string().into(),
            },
            value: Some(Expression::Identifier(Identifier {
                token: Token {
                    kind: TokenType::IDENT,
                    literal: "anotherVar".to_string().into(),
                },
                value: "anotherVar".to_string().into(),
            })),
        }],
    };
    assert_eq!(prog.as_string(), "let myVar = anotherVar;");
}
