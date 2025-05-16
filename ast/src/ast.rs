use lexer::{Token, TokenType};
use std::rc::Rc;

#[derive(Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: Rc<String>,
}

pub enum Expression {
    Identifier(Identifier),
    Statement(Statement),
}

pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        value: Option<Box<Expression>>,
    },
    Return {
        token: Token,
        value: Option<Box<Expression>>,
    },
    Expression {
        token: Token,
        value: Option<Box<Expression>>,
    },
}

pub trait Node {
    fn token_literal(&self) -> Rc<String>;
    fn as_string(&self) -> String;
}

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
                out.push_str(&*token.literal);
                out.push_str(" ");
                out.push_str(&*name.value);
                out.push_str(" = ");
                if let Some(expr) = &*value {
                    out.push_str(&expr.as_string());
                }
                out.push_str(";");
            }
            Statement::Return { token, value } => {
                out.push_str(&*token.literal);
                out.push_str(" ");
                if let Some(ret_val) = &*value {
                    out.push_str(&ret_val.as_string());
                }
                out.push_str(";");
            }
            Statement::Expression { value, .. } => {
                if let Some(expression) = &*value {
                    out.push_str(&expression.as_string());
                }
            }
        }
        out
    }
}

impl Node for Expression {
    fn token_literal(&self) -> Rc<String> {
        match self {
            Expression::Identifier(ident) => ident.token_literal(),
            Expression::Statement(Statement::Expression { token, .. })
            | Expression::Statement(Statement::Let { token, .. })
            | Expression::Statement(Statement::Return { token, .. }) => token.literal.clone(),
        }
    }
    fn as_string(&self) -> String {
        match self {
            Expression::Identifier(Identifier { value, .. }) => value.to_string(),
            Expression::Statement(Statement::Expression { value, .. })
            | Expression::Statement(Statement::Let { value, .. })
            | Expression::Statement(Statement::Return { value, .. }) => {
                if let Some(expression) = value {
                    expression.as_string()
                } else {
                    String::new()
                }
            }
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
            value: Some(Box::new(Expression::Identifier(Identifier {
                token: Token {
                    kind: TokenType::IDENT,
                    literal: "anotherVar".to_string().into(),
                },
                value: "anotherVar".to_string().into(),
            }))),
        }],
    };
    assert_eq!(prog.as_string(), "let myVar = anotherVar;");
}
