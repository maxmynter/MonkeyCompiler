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
        if self.statements.len() > 0 {
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
impl<'a> Statement for LetStatement<'a> {
    fn statement_node(&self) {}
}

impl<'a> Node for LetStatement<'a> {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}
