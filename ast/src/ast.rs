use lexer::Token;

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

pub trait Node {
    fn token_literal(&self) -> String;
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
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(let_stmt) => let_stmt.token.literal.clone(),
            Statement::Return(return_stmt) => return_stmt.token.literal.clone(),
            Statement::Expression(expression_stmt) => expression_stmt.token.literal.clone(),
        }
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        let Expression::Identifier(ident) = &self;
        ident.token_literal()
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
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
}
