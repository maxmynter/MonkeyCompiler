use core::panic::{self, PanicInfo};
use std::collections::HashMap;

use ast::{Expression, Identifier, Node, Program, Statement};
use lexer::{Lexer, Token, TokenType};

type PrefixParseFn<'a> = for<'b> fn(&'b Parser<'a>) -> Expression;
type InfixParseFn<'a> = for<'b> fn(&'b Parser<'a>, Expression) -> Expression;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
    INT,
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > OR
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -x OR !x
    CALL,        // MYfUNCTION(x)
}

impl Precedence {
    fn numeric(self) -> u8 {
        self as u8
    }
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    curr: Token,
    peek: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn<'a>>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn<'a>>,
}

impl<'a> Parser<'a> {
    fn new(mut lexer: Lexer<'a>) -> Self {
        let curr = lexer.next_token();
        let peek = lexer.next_token();

        let mut parser = Parser {
            lexer,
            curr,
            peek,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::IDENT, Parser::parse_identifier);

        parser
    }

    fn parse_identifier(&self) -> Expression {
        Expression::Identifier(ast::Identifier {
            token: self.curr.clone(),
            value: self.curr.literal.clone(),
        })
    }

    fn register_prefix(&mut self, token: TokenType, prefix_parse_fn: PrefixParseFn<'a>) {
        self.prefix_parse_fns.insert(token, prefix_parse_fn);
    }

    fn register_infix(&mut self, token: TokenType, infix_parse_fn: InfixParseFn<'a>) {
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
        while !self.curr_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::Return { token, value: None })
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

        Some(Statement::Let {
            token,
            name,
            value: None,
        })
    }

    fn parse_expression(&mut self, _precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fns[&self.curr.kind];
        Some(prefix(self))
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr.kind {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = Statement::Expression {
            token: self.curr.clone(),
            value: self
                .parse_expression(Precedence::LOWEST)
                .map(|exp| Box::new(exp)),
        };

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(expr)
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
    if let Statement::Let {
        token,
        name: identifier,
        ..
    } = stmt
    {
        if *token.literal != "let" {
            println!("statement token literal not `let`, got={}", token.literal);
            return false;
        }

        if *identifier.value != name {
            println!("let_stmt.name.value not {}, got={}", name, identifier.value);
            return false;
        }

        if *identifier.token_literal() != name {
            println!(
                "let_stmt.name.token_literal not `{}`, got={}",
                name,
                identifier.token_literal()
            );
            return false;
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
            Statement::Return { token, .. } => {
                assert_eq!(*token.literal, "return")
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
    if let Statement::Expression { value, .. } = stmt {
        if let Some(val) = value {
            if let Expression::Identifier(Identifier { token, value }) = &**val {
                assert_eq!(**value, "foobar");
                assert_eq!(*token.literal, "foobar");
            } else {
                panic!("Expression is not an Identifier");
            }
        } else {
            panic!("Expression statement has no value");
        }
    } else {
        panic!("Is not expression statement")
    }
}
