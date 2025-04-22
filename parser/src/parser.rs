use ast::{Identifier, Node, Program, Statement};
use lexer::{Lexer, Token, TokenType};

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

    fn next_token(&mut self) {
        self.curr = std::mem::replace(&mut self.peek, self.lexer.next_token());
    }

    fn parse_statement(&mut self) -> Statement {
        todo!()
    }

    fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];
        while self.curr.kind != TokenType::EOF {
            let stmt = self.parse_statement();
            statements.push(stmt);
            self.next_token();
        }
        Program { statements }
    }
}

#[test]
fn test_let_parsing() {
    let input = "let x = 5;
let y = 10;
let foobar = 838383;
";
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    assert_eq!(program.statements.len(), 3);

    let expected_identifiers = ["x", "y", "foobar"];

    for (i, tt) in expected_identifiers.iter().enumerate() {
        let stmt = &program.statements[i];
        assert!(
            test_let_stmt(stmt, tt),
            "Let statemtent parsing failed: {}",
            i
        );
    }
}

fn test_let_stmt(stmt: &ast::Statement, name: &str) -> bool {
    if let Statement::Let(let_stmt) = stmt {
        if let_stmt.token_literal() != "let" {
            println!(
                "statement token literal not `let`, got={}",
                let_stmt.token_literal()
            );
            return false;
        }
        if let_stmt.name.value != name {
            println!(
                "let_stmt.name.token_literal not {}, got={}",
                name,
                let_stmt.name.token_literal()
            );
            return false;
        }
        if let_stmt.name.token_literal() != name {
            println!(
                "let_stmt.name.token_literal not `{}`, got={}",
                name,
                let_stmt.name.token_literal()
            );
        }
        true
    } else {
        println!("Statement is not a LetStatement");
        false
    }
}
