use lexer::{Token, TokenType};

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

pub trait Node {
    fn token_literal(&self) -> String;
    fn as_string(&self) -> String;
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            String::new()
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
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(let_stmt) => let_stmt.token.literal.clone(),
            Statement::Return(return_stmt) => return_stmt.token.literal.clone(),
            Statement::Expression(expression_stmt) => expression_stmt.token.literal.clone(),
        }
    }

    fn as_string(&self) -> String {
        let mut out = String::new();
        match self {
            Statement::Let(stmt) => {
                out.push_str(&stmt.token.literal);
                out.push_str(" ");
                out.push_str(&stmt.name.value);
                out.push_str(" = ");
                if let Some(expr) = &stmt.value {
                    out.push_str(&expr.as_string());
                }
                out.push_str(";");
            }
            Statement::Return(return_stmt) => {
                out.push_str(&return_stmt.token.literal);
                out.push_str(" ");
                if let Some(ret_val) = &return_stmt.value {
                    out.push_str(&ret_val.as_string());
                }
                out.push_str(";");
            }
            Statement::Expression(expr_stmt) => {
                if let Some(expression) = &expr_stmt.value {
                    out.push_str(&expression.as_string());
                }
            }
        }
        out
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.token_literal(),
            Expression::Statement(expr_statement) => expr_statement.token.literal.clone(),
        }
    }
    fn as_string(&self) -> String {
        match self {
            Expression::Identifier(Identifier { value, .. }) => value.to_string(),
            Expression::Statement(ExpressionStatement { value, .. }) => {
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
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_string(&self) -> String {
        self.value.to_string()
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<Box<Expression>>,
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<Expression>>,
}

pub struct ExpressionStatement {
    pub token: Token,
    pub value: Option<Box<Expression>>,
}

pub enum Expression {
    Identifier(Identifier),
    Statement(ExpressionStatement),
}

#[test]
fn test_ast() {
    let prog = Program {
        statements: vec![Statement::Let(LetStatement {
            token: Token {
                kind: TokenType::LET,
                literal: "let".to_string(),
            },
            name: Identifier {
                token: Token {
                    kind: TokenType::IDENT,
                    literal: "myVar".to_string(),
                },
                value: "myVar".to_string(),
            },
            value: Some(Box::new(Expression::Identifier(Identifier {
                token: Token {
                    kind: TokenType::IDENT,
                    literal: "anotherVar".to_string(),
                },
                value: "anotherVar".to_string(),
            }))),
        })],
    };
    assert_eq!(prog.as_string(), "let myVar = anotherVar;");
}
