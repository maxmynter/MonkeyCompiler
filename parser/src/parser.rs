use lazy_static::lazy_static;
use std::collections::HashMap;
use std::rc::Rc;

use ast::{
    Expression, Identifier, InfixExpression, IntegerLiteral, Node, PrefixExpression, Program,
    Statement,
};
use lexer::{Lexer, Token, TokenType};

type PrefixParseFn<'a> = for<'b> fn(&'b mut Parser<'a>) -> Expression;
type InfixParseFn<'a> = for<'b> fn(&'b mut Parser<'a>, Expression) -> Expression;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum PRECEDENCE {
    INT,
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > OR
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -x OR !x
    CALL,        // MYfUNCTION(x)
}

impl PRECEDENCE {
    fn numeric(self) -> u8 {
        self as u8
    }
}

lazy_static! {
    pub static ref PRECEDENCES: HashMap<TokenType, PRECEDENCE> = {
        [
            (TokenType::EQ, PRECEDENCE::EQUALS),
            (TokenType::UNEQ, PRECEDENCE::EQUALS),
            (TokenType::LT, PRECEDENCE::LESSGREATER),
            (TokenType::GT, PRECEDENCE::LESSGREATER),
            (TokenType::PLUS, PRECEDENCE::SUM),
            (TokenType::MINUS, PRECEDENCE::SUM),
            (TokenType::SLASH, PRECEDENCE::PRODUCT),
            (TokenType::ASTERISK, PRECEDENCE::PRODUCT),
        ]
        .iter()
        .cloned()
        .collect()
    };
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
        parser.register_prefix(TokenType::INT, Parser::parse_integer_literal);
        parser.register_prefix(TokenType::BANG, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::MINUS, Parser::parse_prefix_expression);

        parser.register_infix(TokenType::PLUS, Parser::parse_infix_expression);
        parser.register_infix(TokenType::MINUS, Parser::parse_infix_expression);
        parser.register_infix(TokenType::SLASH, Parser::parse_infix_expression);
        parser.register_infix(TokenType::ASTERISK, Parser::parse_infix_expression);
        parser.register_infix(TokenType::EQ, Parser::parse_infix_expression);
        parser.register_infix(TokenType::UNEQ, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LT, Parser::parse_infix_expression);
        parser.register_infix(TokenType::GT, Parser::parse_infix_expression);

        parser
    }

    fn parse_integer_literal(&mut self) -> Expression {
        Expression::IntegerLiteral(IntegerLiteral {
            token: self.curr.clone(),
            value: Rc::new(self.curr.literal.parse::<i64>().unwrap()),
        })
    }

    fn peek_precedence(&self) -> PRECEDENCE {
        if PRECEDENCES.contains_key(&self.peek.kind) {
            return PRECEDENCES[&self.peek.kind];
        }
        PRECEDENCE::LOWEST
    }

    fn curr_precedence(&self) -> PRECEDENCE {
        if PRECEDENCES.contains_key(&self.curr.kind) {
            return PRECEDENCES[&self.curr.kind];
        }
        PRECEDENCE::LOWEST
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let token = self.curr.clone();
        let operator = self.curr.literal.to_string();
        let right_precedence = self.curr_precedence();
        self.next_token();
        let right = self
            .parse_expression(right_precedence)
            .unwrap_or_else(|| panic!("Could not parse expression around {}", operator));
        Expression::InfixExpression(InfixExpression {
            token,
            operator,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let token = self.curr.clone();
        let operator = self.curr.literal.to_string();
        self.next_token();
        let right = self.parse_expression(PRECEDENCE::PREFIX).unwrap();
        Expression::PrefixExpression(PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        })
    }

    fn parse_identifier(&mut self) -> Expression {
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
        self.peek.kind == *t
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

    fn parse_expression(&mut self, precedence: PRECEDENCE) -> Option<Expression> {
        let mut prefix;
        if let Some(prefix_parse_fn) = self.prefix_parse_fns.get(&self.curr.kind) {
            prefix = prefix_parse_fn(self);
        } else {
            panic!("Cannot parse prefix: {}", self.curr.literal)
        }

        while !self.peek_token_is(&TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            if let Some(infix_parse_fn) = self.infix_parse_fns.get(&self.peek.kind).cloned() {
                self.next_token();
                prefix = infix_parse_fn(self, prefix);
            } else {
                return Some(prefix);
            };
        }
        Some(prefix)
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
            value: self.parse_expression(PRECEDENCE::LOWEST).map(Box::new),
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

#[test]
fn test_integer() {
    let input = String::from("5;");
    let l = Lexer::new(&input);
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(p);
    assert_eq!(program.statements.len(), 1);
    let stmt = &program.statements[0];
    if let Statement::Expression { value, .. } = stmt {
        if let Some(val) = value {
            if let Expression::IntegerLiteral(IntegerLiteral { token, value }) = &**val {
                assert_eq!(**value, 5);
                assert_eq!(*token.literal, "5");
            } else {
                panic!("Expression is not an integer literal");
            }
        } else {
            panic!("Expression statement has no value");
        }
    } else {
        panic!("Is not an integer literal statement");
    }
}

fn test_integer_literal(expr: Expression, val: i64) {
    let Expression::IntegerLiteral(il) = expr else {
        panic!()
    };
    assert_eq!(val, *il.value);
    assert_eq!(val.to_string(), *il.token.literal);
}

#[test]
fn test_prefix_expressions() {
    struct PrefixExpressionTest<'a> {
        input: &'a str,
        operator: &'a str,
        integer_value: i64,
    }
    let prefix_tests = vec![
        PrefixExpressionTest {
            input: "!5;",
            operator: "!",
            integer_value: 5,
        },
        PrefixExpressionTest {
            input: "-15;",
            operator: "-",
            integer_value: 15,
        },
    ];

    for tt in prefix_tests {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);

        assert_eq!(program.statements.len(), 1);
        let Statement::Expression { value, .. } = &program.statements[0] else {
            panic!()
        };
        match *(value.clone().unwrap()) {
            Expression::PrefixExpression(prefix) => {
                let PrefixExpression {
                    operator, right, ..
                } = prefix;
                assert_eq!(operator, tt.operator);
                test_integer_literal(*right, tt.integer_value);
            }
            _ => panic!(),
        }
    }
}

#[test]
fn test_infix_expressions() {
    struct InfixExpressionTest<'a> {
        input: &'a str,
        operator: &'a str,
        left: i64,
        right: i64,
    }
    let infix_tests = vec![
        InfixExpressionTest {
            input: "5+5;",
            operator: "+",
            left: 5,
            right: 5,
        },
        InfixExpressionTest {
            input: "5-5;",
            operator: "-",
            left: 5,
            right: 5,
        },
        InfixExpressionTest {
            input: "5*5;",
            operator: "*",
            left: 5,
            right: 5,
        },
        InfixExpressionTest {
            input: "5/5;",
            operator: "/",
            left: 5,
            right: 5,
        },
        InfixExpressionTest {
            input: "5>5;",
            operator: ">",
            left: 5,
            right: 5,
        },
        InfixExpressionTest {
            input: "5<5;",
            operator: "<",
            left: 5,
            right: 5,
        },
        InfixExpressionTest {
            input: "5==5;",
            operator: "==",
            left: 5,
            right: 5,
        },
        InfixExpressionTest {
            input: "5!=5;",
            operator: "!=",
            left: 5,
            right: 5,
        },
    ];

    for tt in infix_tests {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 1);
        let Statement::Expression { value, .. } = &program.statements[0] else {
            panic!("Is not infix expression");
        };
        match *(value.clone().unwrap()) {
            Expression::InfixExpression(infix) => {
                let InfixExpression {
                    token: _,
                    operator,
                    right,
                    left,
                } = infix;
                test_integer_literal(*left, tt.left);
                test_integer_literal(*right, tt.right);
                assert_eq!(operator, tt.operator);
            }
            _ => panic!(),
        }
    }
}
