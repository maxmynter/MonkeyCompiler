use core::panic::{self, PanicInfo};
use std::collections::HashMap;

use ast::{
    Expression, ExpressionStatement, Identifier, LetStatement, Node, Program, ReturnStatement,
    Statement,
};
use lexer::{Lexer, Token, TokenType};

type PrefixParseFn = fn() -> Expression;
type InfixParseFn = fn(Expression) -> Expression;

struct Parser<'a> {
    lexer: Lexer<'a>,
    curr: Token,
    peek: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl<'a> Parser<'a> {
    fn new(mut lexer: Lexer<'a>) -> Self {
        let curr = lexer.next_token();
        let peek = lexer.next_token();
        Parser {
            lexer,
            curr,
            peek,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        }
    }

    fn register_prefix(&mut self, token: TokenType, prefix_parse_fn: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, prefix_parse_fn);
    }

    fn register_infix(&mut self, token: TokenType, infix_parse_fn: InfixParseFn) {
        self.infix_parse_fns.insert(token, infix_parse_fn);
    }

    fn next_token(&mut self) {
        self.curr = std::mem::replace(&mut self.peek, self.lexer.next_token());
    }

    fn peek_token_is(&mut self, t: &TokenType) -> bool {
        let token_match = self.peek.kind == *t;
        if !token_match {
            self.peek_error(t);
        }
        token_match
    }

    fn curr_token_is(&self, t: &TokenType) -> bool {
        self.curr.kind == *t
    }

    fn expect_peek_token(&mut self, token: TokenType) -> bool {
        if self.peek_token_is(&token) {
            self.next_token();
            true
        } else {
            self.peek_error(&token);
            false
        }
    }

    fn peek_error(&mut self, expected_token: &TokenType) {
        let message = format!(
            "expected next token to be {:?}, got {:?}",
            *expected_token, self.peek.kind
        );
        self.errors.push(message);
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.curr.clone();
        while self.curr_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        let return_statment = ReturnStatement { token, value: None };
        Some(Statement::Return(return_statment))
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.curr.clone();

        if !self.expect_peek_token(TokenType::IDENT) {
            return None;
        }

        let name = Identifier {
            token: self.curr.clone(),
            value: self.curr.literal.clone(),
        };

        if !self.expect_peek_token(TokenType::ASSIGN) {
            return None;
        }

        // TODO: skip expressions until ;
        while !self.curr_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        let stmt = LetStatement {
            token,
            name,
            value: None,
        };
        Some(Statement::Let(stmt))
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr.kind {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let stmt = self.curr;
        self.parse_expression(LOWEST);
        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
    }

    fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];
        while self.curr.kind != TokenType::EOF {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
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

    check_parse_errors(p);

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

fn test_let_stmt(stmt: &Statement, name: &str) -> bool {
    if let Statement::Let(let_stmt) = stmt {
        if stmt.token_literal() != "let" {
            println!(
                "statement token literal not `let`, got={}",
                stmt.token_literal()
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

fn check_parse_errors(p: Parser) {
    println!("Parser has {} errors", p.errors.len());
    for msg in &p.errors {
        eprintln!("Error: {}", msg);
    }
    assert_eq!(p.errors.len(), 0);
}

#[test]
fn test_return_statement() {
    let input = "
return 5;
return 10;
return 993322;
";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(p);
    assert_eq!(program.statements.len(), 3);

    for stmt in &program.statements {
        match stmt {
            Statement::Return(ReturnStatement { token, .. }) => {
                assert_eq!(token.literal, "return")
            }
            _ => panic!("Did not get `return` statement"),
        }
    }
}

#[test]
fn test_identifier() {
    let input = String::from("foobar;");
    let l = Lexer::new(&input);
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(p);
    assert_eq!(program.statements.len(), 1);
    let stmt = &program.statements[0];
    if let Statement::Expression(expr_stmt) = stmt {
        if let Some(val) = &expr_stmt.value {
            if let Expression::Identifier(Identifier { token, value }) = &**val {
                assert_eq!(value, "foobar");
                assert_eq!(token.literal, "foobar");
            }
        }
    } else {
        panic!("Is not expression statement")
    }
}
