use lexer::Token;

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self) {}
}

pub trait Expression: Node {
    fn expression_node(&self) {}
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
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

struct Identifier {
    token: Token,
    value: String,
}

struct LetStatement<'a> {
    token: Token,
    name: Identifier,
    value: Box<dyn Expression + 'a>,
}
impl Statement for LetStatement<'_> {
    fn statement_node(&self) {}
}

impl Node for LetStatement<'_> {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}
