use lazy_static::lazy_static;
use std::collections::HashMap;
use std::rc::Rc;

use ast::{
    Boolean, Expression, Identifier, InfixExpression, IntegerLiteral, Node, PrefixExpression,
    Program, Statement,
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
        parser.register_prefix(TokenType::TRUE, Parser::parse_boolean);
        parser.register_prefix(TokenType::FALSE, Parser::parse_boolean);

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

    fn parse_boolean(&mut self) -> Expression {
        Expression::Boolean(Boolean {
            token: self.curr.clone(),
            value: self.curr_token_is(&TokenType::TRUE),
        })
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

fn unwrap_expression(stmt: &Statement) -> &Expression {
    if let Statement::Expression { value, .. } = stmt {
        if let Some(val) = value {
            val
        } else {
            panic!("Expression statement has no value")
        }
    } else {
        panic!("Statement is not an expression statment")
    }
}

fn check_parse_errors(p: Parser) {
    println!("Parser has {} errors", p.errors.len());
    for msg in &p.errors {
        eprintln!("Error: {}", msg);
    }
    assert_eq!(p.errors.len(), 0);
}

fn prepare_program_for_test(input: &str) -> Program {
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(p);
    program
}

trait TestLiteral {
    fn test_literal(&self, expr: &Expression) -> bool;
}

impl TestLiteral for i64 {
    fn test_literal(&self, expr: &Expression) -> bool {
        if let Expression::IntegerLiteral(il) = expr {
            *il.value == *self && il.token_literal() == self.to_string().into()
        } else {
            false
        }
    }
}

impl TestLiteral for &str {
    fn test_literal(&self, expr: &Expression) -> bool {
        if let Expression::Identifier(id) = expr {
            id.value.as_str() == *self && id.token_literal().as_str() == *self
        } else {
            false
        }
    }
}

impl TestLiteral for bool {
    fn test_literal(&self, expr: &Expression) -> bool {
        if let Expression::Boolean(b) = expr {
            b.value == *self && b.token_literal() == self.to_string().into()
        } else {
            false
        }
    }
}

impl TestLiteral for Expression {
    fn test_literal(&self, expr: &Expression) -> bool {
        match (self, expr) {
            (Expression::IntegerLiteral(expected), Expression::IntegerLiteral(actual)) => {
                *expected.value == *actual.value
            }
            (Expression::Boolean(expected), Expression::Boolean(actual)) => {
                expected.value == actual.value
            }
            (Expression::Identifier(expected), Expression::Identifier(actual)) => {
                expected.value == actual.value
            }
            _ => false,
        }
    }
}

fn test_literal_expression<T: TestLiteral>(expr: &Expression, expected: T) -> bool {
    expected.test_literal(expr)
}

fn test_infix_expression<L, R>(
    expr: &Expression,
    left_expected: L,
    operator: &str,
    right_expected: R,
) -> bool
where
    L: TestLiteral,
    R: TestLiteral,
{
    if let Expression::InfixExpression(infix) = expr {
        test_literal_expression(&infix.left, left_expected)
            && infix.operator == operator
            && test_literal_expression(&infix.right, right_expected)
    } else {
        false
    }
}

fn test_prefix_expression<P: TestLiteral>(
    expr: &Expression,
    operator: &str,
    right_expected: P,
) -> bool {
    if let Expression::PrefixExpression(prefix) = expr {
        prefix.operator == operator && test_literal_expression(&prefix.right, right_expected)
    } else {
        false
    }
}

fn test_integer_literal(expr: &Expression, expected: i64) -> bool {
    test_literal_expression(expr, expected)
}

fn test_identifier(expr: &Expression, expected: &str) -> bool {
    test_literal_expression(expr, expected)
}

fn test_boolean_literal(expr: &Expression, expected: bool) -> bool {
    test_literal_expression(expr, expected)
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

#[test]
fn test_let_parsing() {
    let input = "let x = 5;
let y = 10;
let foobar = 838383;
";
    let program = prepare_program_for_test(input);
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

#[test]
fn test_return_statement() {
    let input = "
return 5;
return 10;
return 993322;
";
    let program = prepare_program_for_test(input);
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
fn test_identifiers() {
    let input = String::from("foobar;");
    let program = prepare_program_for_test(&input);
    assert_eq!(program.statements.len(), 1);
    let expr = unwrap_expression(&program.statements[0]);
    assert!(test_literal_expression(expr, "foobar"));
}

#[test]
fn test_integer() {
    let input = String::from("5;");
    let program = prepare_program_for_test(&input);
    assert_eq!(program.statements.len(), 1);
    let expr = unwrap_expression(&program.statements[0]);
    test_literal_expression(expr, 5i64);
}

#[test]
fn test_prefix_expressions() {
    struct PrefixExpressionTest<'a> {
        input: &'a str,
        operator: &'a str,
        integer_value: Expression,
    }
    let prefix_tests = vec![
        PrefixExpressionTest {
            input: "!5;",
            operator: "!",
            integer_value: Expression::from_int(5),
        },
        PrefixExpressionTest {
            input: "-15;",
            operator: "-",
            integer_value: Expression::from_int(15),
        },
    ];

    for tt in prefix_tests {
        let program = prepare_program_for_test(tt.input);
        assert_eq!(program.statements.len(), 1);
        let expr = unwrap_expression(&program.statements[0]);
        assert!(
            test_prefix_expression(expr, tt.operator, tt.integer_value),
            "Expected prefix expression, got: {:?}",
            expr
        );
    }
}

#[test]
fn test_infix_expressions() {
    struct InfixExpressionTest<'a> {
        input: &'a str,
        operator: &'a str,
        left: Expression,
        right: Expression,
    }
    let infix_tests = vec![
        InfixExpressionTest {
            input: "5+5;",
            operator: "+",
            left: Expression::from_int(5),
            right: Expression::from_int(5),
        },
        InfixExpressionTest {
            input: "5-5;",
            operator: "-",
            left: Expression::from_int(5),
            right: Expression::from_int(5),
        },
        InfixExpressionTest {
            input: "5*5;",
            operator: "*",
            left: Expression::from_int(5),
            right: Expression::from_int(5),
        },
        InfixExpressionTest {
            input: "5/5;",
            operator: "/",
            left: Expression::from_int(5),
            right: Expression::from_int(5),
        },
        InfixExpressionTest {
            input: "5>5;",
            operator: ">",
            left: Expression::from_int(5),
            right: Expression::from_int(5),
        },
        InfixExpressionTest {
            input: "5<5;",
            operator: "<",
            left: Expression::from_int(5),
            right: Expression::from_int(5),
        },
        InfixExpressionTest {
            input: "5==5;",
            operator: "==",
            left: Expression::from_int(5),
            right: Expression::from_int(5),
        },
        InfixExpressionTest {
            input: "5!=5;",
            operator: "!=",
            left: Expression::from_int(5),
            right: Expression::from_int(5),
        },
        InfixExpressionTest {
            input: "true == true",
            operator: "==",
            left: Expression::from_bool(true),
            right: Expression::from_bool(true),
        },
        InfixExpressionTest {
            input: "true != false;",
            operator: "!=",
            left: Expression::from_bool(true),
            right: Expression::from_bool(false),
        },
        InfixExpressionTest {
            input: "false == false;",
            operator: "==",
            left: Expression::from_bool(false),
            right: Expression::from_bool(false),
        },
    ];

    for tt in infix_tests {
        let program = prepare_program_for_test(tt.input);
        assert_eq!(program.statements.len(), 1);
        let expr = unwrap_expression(&program.statements[0]);
        assert!(
            test_infix_expression(expr, tt.left, tt.operator, tt.right),
            "Failed infix test for input: {}",
            tt.input
        );
    }
}

#[test]
fn test_operator_precedence_parsing() {
    struct OperatorPrecedenceTest<'a> {
        input: &'a str,
        expected: &'a str,
    }

    let tests = vec![
        OperatorPrecedenceTest {
            input: "-a * b",
            expected: "((-a) * b)",
        },
        OperatorPrecedenceTest {
            input: "!-a",
            expected: "(!(-a))",
        },
        OperatorPrecedenceTest {
            input: "a + b + c",
            expected: "((a + b) + c)",
        },
        OperatorPrecedenceTest {
            input: "a + b - c",
            expected: "((a + b) - c)",
        },
        OperatorPrecedenceTest {
            input: "a * b * c",
            expected: "((a * b) * c)",
        },
        OperatorPrecedenceTest {
            input: "a * b / c",
            expected: "((a * b) / c)",
        },
        OperatorPrecedenceTest {
            input: "a + b / c",
            expected: "(a + (b / c))",
        },
        OperatorPrecedenceTest {
            input: "a + b * c + d / e - f",
            expected: "(((a + (b * c)) + (d / e)) - f)",
        },
        OperatorPrecedenceTest {
            input: "3 + 4; -5 * 5",
            expected: "(3 + 4)((-5) * 5)",
        },
        OperatorPrecedenceTest {
            input: "5 > 4 == 3 < 4",
            expected: "((5 > 4) == (3 < 4))",
        },
        OperatorPrecedenceTest {
            input: "5 < 4 != 3 > 4",
            expected: "((5 < 4) != (3 > 4))",
        },
        OperatorPrecedenceTest {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        OperatorPrecedenceTest {
            input: "true",
            expected: "true",
        },
        OperatorPrecedenceTest {
            input: "false",
            expected: "false",
        },
        OperatorPrecedenceTest {
            input: "3 > 5 == false",
            expected: "((3 > 5) == false)",
        },
        OperatorPrecedenceTest {
            input: "3 < 5 == true",
            expected: "((3 < 5) == true)",
        },
    ];

    for tt in tests {
        let program = prepare_program_for_test(tt.input);
        let actual = program.to_string();
        assert_eq!(actual, tt.expected, "for input: {}", tt.input);
    }
}
