use ast::Program;
use lexer::{Lexer, Token};

struct Parser<'a> {
    lexer: Lexer<'a>,
    curr: Token,
    peek: Token,
}

impl<'a> Parser<'a> {
    fn new(mut lexer: Lexer<'a>) -> Self {
        let curr = lexer.next_token();
        let peek = lexer.next_token();
        Parser { lexer, curr, peek }
    }

    fn next_token(&'a mut self) {
        self.curr = std::mem::replace(&mut self.peek, self.lexer.next_token());
    }

    fn parse_program(&mut self) -> Program {
        todo!()
    }
}
