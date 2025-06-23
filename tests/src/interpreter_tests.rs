#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]
use crate::utils::prepare_program_for_test;
use ast::{
    ArrayLiteral, CallExpression, Expression, FunctionLiteral, HashLiteral, Identifier,
    IfExpression, IntegerLiteral, Node, Program, Statement, StringLiteral,
};
use lexer::Lexer;
use lexer::{Token, TokenType};
use object::{
    CoerceObject, Environment, EvalError, FALSE, HashKey, NULL, Object, ObjectTraits, TRUE,
};
use parser::Parser;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

#[test]
fn test_ast() {
    let prog = Program {
        statements: vec![Statement::Let {
            token: Token {
                kind: TokenType::LET,
                literal: "let".to_string().into(),
            },
            name: Identifier {
                token: Token {
                    kind: TokenType::IDENT,
                    literal: "myVar".to_string().into(),
                },
                value: "myVar".to_string().into(),
            },
            value: Expression::Identifier(Identifier {
                token: Token {
                    kind: TokenType::IDENT,
                    literal: "anotherVar".to_string().into(),
                },
                value: "anotherVar".to_string().into(),
            }),
        }],
    };
    assert_eq!(prog.as_string(), "let myVar = anotherVar;");
}

#[test]
fn test_next_token() {
    let input = String::from("=+(){},;");
    let expected = [
        Token {
            kind: TokenType::ASSIGN,
            literal: "=".to_string().into(),
        },
        Token {
            kind: TokenType::PLUS,
            literal: "+".to_string().into(),
        },
        Token {
            kind: TokenType::LPAREN,
            literal: "(".to_string().into(),
        },
        Token {
            kind: TokenType::RPAREN,
            literal: ")".to_string().into(),
        },
        Token {
            kind: TokenType::LBRACE,
            literal: "{".to_string().into(),
        },
        Token {
            kind: TokenType::RBRACE,
            literal: "}".to_string().into(),
        },
        Token {
            kind: TokenType::COMMA,
            literal: ",".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
    ];
    let mut lexer = Lexer::new(&input);
    for i in 0..expected.len() {
        assert_eq!(lexer.next_token(), expected[i])
    }
}

#[test]
fn test_parse_code() {
    let input = String::from(
        "let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);

        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
        return true;
        } else {
        return false;
        }

        10 == 10;
        10 != 9;
        \"foobar\"
        \"foo bar\"
        [1, 2];
        {\"foo\": \"bar\"}
        ",
    );
    let expected = vec![
        // let five = 5;
        Token {
            kind: TokenType::LET,
            literal: "let".to_string().into(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "five".to_string().into(),
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=".to_string().into(),
        },
        Token {
            kind: TokenType::INT,
            literal: "5".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // let ten = 10;
        Token {
            kind: TokenType::LET,
            literal: "let".to_string().into(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "ten".to_string().into(),
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=".to_string().into(),
        },
        Token {
            kind: TokenType::INT,
            literal: "10".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // let add = fn(x, y) {
        Token {
            kind: TokenType::LET,
            literal: "let".to_string().into(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "add".to_string().into(),
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=".to_string().into(),
        },
        Token {
            kind: TokenType::FUNCTION,
            literal: "fn".to_string().into(),
        },
        Token {
            kind: TokenType::LPAREN,
            literal: "(".to_string().into(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "x".to_string().into(),
        },
        Token {
            kind: TokenType::COMMA,
            literal: ",".to_string().into(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "y".to_string().into(),
        },
        Token {
            kind: TokenType::RPAREN,
            literal: ")".to_string().into(),
        },
        Token {
            kind: TokenType::LBRACE,
            literal: "{".to_string().into(),
        },
        // x + y;
        Token {
            kind: TokenType::IDENT,
            literal: "x".to_string().into(),
        },
        Token {
            kind: TokenType::PLUS,
            literal: "+".to_string().into(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "y".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // };
        Token {
            kind: TokenType::RBRACE,
            literal: "}".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // let result = add(five, ten);
        Token {
            kind: TokenType::LET,
            literal: "let".to_string().into(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "result".to_string().into(),
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=".to_string().into(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "add".to_string().into(),
        },
        Token {
            kind: TokenType::LPAREN,
            literal: "(".to_string().into(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "five".to_string().into(),
        },
        Token {
            kind: TokenType::COMMA,
            literal: ",".to_string().into(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "ten".to_string().into(),
        },
        Token {
            kind: TokenType::RPAREN,
            literal: ")".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // !-/*5;
        Token {
            kind: TokenType::BANG,
            literal: "!".to_string().into(),
        },
        Token {
            kind: TokenType::MINUS,
            literal: "-".to_string().into(),
        },
        Token {
            kind: TokenType::SLASH,
            literal: "/".to_string().into(),
        },
        Token {
            kind: TokenType::ASTERISK,
            literal: "*".to_string().into(),
        },
        Token {
            kind: TokenType::INT,
            literal: "5".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // 5 < 10 > 5;
        Token {
            kind: TokenType::INT,
            literal: "5".to_string().into(),
        },
        Token {
            kind: TokenType::LT,
            literal: "<".to_string().into(),
        },
        Token {
            kind: TokenType::INT,
            literal: "10".to_string().into(),
        },
        Token {
            kind: TokenType::GT,
            literal: ">".to_string().into(),
        },
        Token {
            kind: TokenType::INT,
            literal: "5".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // if (5 < 10) {
        Token {
            kind: TokenType::IF,
            literal: "if".to_string().into(),
        },
        Token {
            kind: TokenType::LPAREN,
            literal: "(".to_string().into(),
        },
        Token {
            kind: TokenType::INT,
            literal: "5".to_string().into(),
        },
        Token {
            kind: TokenType::LT,
            literal: "<".to_string().into(),
        },
        Token {
            kind: TokenType::INT,
            literal: "10".to_string().into(),
        },
        Token {
            kind: TokenType::RPAREN,
            literal: ")".to_string().into(),
        },
        Token {
            kind: TokenType::LBRACE,
            literal: "{".to_string().into(),
        },
        // return true;
        Token {
            kind: TokenType::RETURN,
            literal: "return".to_string().into(),
        },
        Token {
            kind: TokenType::TRUE,
            literal: "true".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // } else {
        Token {
            kind: TokenType::RBRACE,
            literal: "}".to_string().into(),
        },
        Token {
            kind: TokenType::ELSE,
            literal: "else".to_string().into(),
        },
        Token {
            kind: TokenType::LBRACE,
            literal: "{".to_string().into(),
        },
        // return false;
        Token {
            kind: TokenType::RETURN,
            literal: "return".to_string().into(),
        },
        Token {
            kind: TokenType::FALSE,
            literal: "false".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // }
        Token {
            kind: TokenType::RBRACE,
            literal: "}".to_string().into(),
        },
        // 10 == 10;
        Token {
            kind: TokenType::INT,
            literal: "10".to_string().into(),
        },
        Token {
            kind: TokenType::EQ,
            literal: "==".to_string().into(),
        },
        Token {
            kind: TokenType::INT,
            literal: "10".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // 10 != 9;
        Token {
            kind: TokenType::INT,
            literal: "10".to_string().into(),
        },
        Token {
            kind: TokenType::UNEQ,
            literal: "!=".to_string().into(),
        },
        Token {
            kind: TokenType::INT,
            literal: "9".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // foobar
        Token {
            kind: TokenType::STRING,
            literal: "foobar".to_string().into(),
        },
        // foo bar
        Token {
            kind: TokenType::STRING,
            literal: "foo bar".to_string().into(),
        },
        // [1, 2];
        Token {
            kind: TokenType::LBRACKET,
            literal: "[".to_string().into(),
        },
        Token {
            kind: TokenType::INT,
            literal: "1".to_string().into(),
        },
        Token {
            kind: TokenType::COMMA,
            literal: ",".to_string().into(),
        },
        Token {
            kind: TokenType::INT,
            literal: "2".to_string().into(),
        },
        Token {
            kind: TokenType::RBRACKET,
            literal: "]".to_string().into(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string().into(),
        },
        // {"foo": "bar"}
        Token {
            kind: TokenType::LBRACE,
            literal: "{".to_string().into(),
        },
        Token {
            kind: TokenType::STRING,
            literal: "foo".to_string().into(),
        },
        Token {
            kind: TokenType::COLON,
            literal: ":".to_string().into(),
        },
        Token {
            kind: TokenType::STRING,
            literal: "bar".to_string().into(),
        },
        Token {
            kind: TokenType::RBRACE,
            literal: "}".to_string().into(),
        },
        // EOF
        Token {
            kind: TokenType::EOF,
            literal: "".to_string().into(),
        },
    ];
    let mut lexer = Lexer::new(&input);
    for i in 0..expected.len() {
        let tok = lexer.next_token();
        println!("{:?} -|- {:?}", tok, expected[i]);
        assert_eq!(tok, expected[i]);
    }
}

fn unwrap_expression(stmt: &Statement) -> &Expression {
    if let Statement::Expression { value, .. } = stmt {
        value
    } else {
        panic!("Statement is not an expression statment")
    }
}

trait TestLiteral {
    fn test_literal(&self, expr: &Expression) -> bool;
}

impl TestLiteral for i64 {
    fn test_literal(&self, expr: &Expression) -> bool {
        if let Expression::IntegerLiteral(il) = expr {
            il.value == *self && il.token_literal() == self.to_string().into()
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
                expected.value == actual.value
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

struct ExpectedLet<'a> {
    name: &'a str,
    value: Expression,
}

fn test_let_stmt(stmt: &Statement, expected: &ExpectedLet) -> bool {
    if let Statement::Let {
        token,
        name: identifier,
        value: expr,
    } = stmt
    {
        if *token.literal != "let" {
            println!("statement token literal not `let`, got={}", token.literal);
            return false;
        }

        if *identifier.value != expected.name {
            println!(
                "let_stmt.name.value not {}, got={}",
                expected.name, identifier.value
            );
            return false;
        }

        if *identifier.token_literal() != expected.name {
            println!(
                "let_stmt.name.token_literal not `{}`, got={}",
                expected.name,
                identifier.token_literal()
            );
            return false;
        }
        if !test_literal_expression(expr, expected.value.clone()) {
            println!(
                "let expression incorrect. Got={}, Expected={}",
                expr, expected.value
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

    let expected = [
        ExpectedLet {
            name: "x",
            value: Expression::IntegerLiteral(IntegerLiteral {
                token: Token {
                    kind: TokenType::INT,
                    literal: Rc::new("5".to_string()),
                },
                value: 5.into(),
            }),
        },
        ExpectedLet {
            name: "y",
            value: Expression::IntegerLiteral(IntegerLiteral {
                token: Token {
                    kind: TokenType::INT,
                    literal: Rc::new("10".to_string()),
                },
                value: 10.into(),
            }),
        },
        ExpectedLet {
            name: "foobar",
            value: Expression::IntegerLiteral(IntegerLiteral {
                token: Token {
                    kind: TokenType::INT,
                    literal: Rc::new("838383".to_string()),
                },
                value: 838383.into(),
            }),
        },
    ];

    for (i, tt) in expected.iter().enumerate() {
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
    let expected_values = [5, 10, 993322];
    let program = prepare_program_for_test(input);
    assert_eq!(program.statements.len(), 3);
    for (i, stmt) in program.statements.iter().enumerate() {
        match stmt {
            Statement::Return { token, value } => {
                assert_eq!(*token.literal, "return");
                test_literal_expression(value, expected_values[i]);
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
    test_integer_literal(expr, 5i64);
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
        OperatorPrecedenceTest {
            input: "1 + (2 + 3) + 4",
            expected: "((1 + (2 + 3)) + 4)",
        },
        OperatorPrecedenceTest {
            input: "(5 + 5) * 2",
            expected: "((5 + 5) * 2)",
        },
        OperatorPrecedenceTest {
            input: "2 / (5 + 5)",
            expected: "(2 / (5 + 5))",
        },
        OperatorPrecedenceTest {
            input: "-(5 + 5)",
            expected: "(-(5 + 5))",
        },
        OperatorPrecedenceTest {
            input: "!(true == true)",
            expected: "(!(true == true))",
        },
        OperatorPrecedenceTest {
            input: "a + add(b * c) + d",
            expected: "((a + add((b * c))) + d)",
        },
        OperatorPrecedenceTest {
            input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        OperatorPrecedenceTest {
            input: "add(a + b + c * d / f + g)",
            expected: "add((((a + b) + ((c * d) / f)) + g))",
        },
        OperatorPrecedenceTest {
            input: "a * [1, 2, 3, 4][b * c] * d",
            expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        },
        OperatorPrecedenceTest {
            input: "add(a * b[2], b[1], 2 * [1, 2][1])",
            expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        },
    ];

    for tt in tests {
        let program = prepare_program_for_test(tt.input);
        let actual = program.to_string();
        assert_eq!(actual, tt.expected, "for input: {}", tt.input);
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x };";
    let program = prepare_program_for_test(input);
    assert!(program.statements.len() == 1);
    let expr = unwrap_expression(&program.statements[0]);
    if let Expression::IfExpression(IfExpression {
        condition,
        consequence,
        alternative,
        ..
    }) = expr
    {
        test_infix_expression(condition, "x", "<", "y");
        assert!(consequence.statements.len() == 1);
        let consequence_expr = unwrap_expression(&consequence.statements[0]);
        test_identifier(consequence_expr, "x");
        assert!(alternative.is_none());
    } else {
        panic!("Expected if expression")
    }
}

#[test]
fn test_parse_if_else_expression() {
    let input = "if (x < y) { x } else { y };";
    let program = prepare_program_for_test(input);
    assert!(program.statements.len() == 1);
    let expr = unwrap_expression(&program.statements[0]);
    if let Expression::IfExpression(IfExpression {
        condition,
        consequence,
        alternative,
        ..
    }) = expr
    {
        test_infix_expression(condition, "x", "<", "y");
        assert!(consequence.statements.len() == 1);
        let consequence_expr = unwrap_expression(&consequence.statements[0]);
        test_identifier(consequence_expr, "x");
        if let Some(else_expr) = alternative {
            assert!(else_expr.statements.len() == 1);
            let alternative_expr = unwrap_expression(&else_expr.statements[0]);
            test_identifier(alternative_expr, "y");
        } else {
            panic!("Error with else expression")
        }
    } else {
        panic!("Expected if expression")
    }
}

#[test]
fn test_function_literal() {
    let input = "fn(x, y) { x + y; };";
    let program = prepare_program_for_test(input);
    assert!(program.statements.len() == 1);
    let expr = unwrap_expression(&program.statements[0]);
    if let Expression::FunctionLiteral(FunctionLiteral {
        parameters, body, ..
    }) = expr
    {
        assert_eq!(parameters.len(), 2);
        test_literal_expression(&parameters[0].as_expression(), "x");
        test_literal_expression(&parameters[1].as_expression(), "y");
        assert_eq!(body.statements.len(), 1);
        let expr = unwrap_expression(&body.statements[0]);
        test_infix_expression(expr, "x", "+", "y");
    } else {
        panic!("Expected a function literal");
    }
}

#[test]
fn test_function_parameter_parsing() {
    struct FnParameterTest<'a> {
        input: &'static str,
        expected: Vec<&'a str>,
    }
    let tests = [
        FnParameterTest {
            input: "fn(){};",
            expected: [].to_vec(),
        },
        FnParameterTest {
            input: "fn(x){};",
            expected: ["x"].to_vec(),
        },
        FnParameterTest {
            input: "fn(x, y, z){};",
            expected: ["x", "y", "z"].to_vec(),
        },
    ];

    for tt in tests {
        let program = prepare_program_for_test(tt.input);
        let expr = unwrap_expression(&program.statements[0]);
        if let Expression::FunctionLiteral(FunctionLiteral { parameters, .. }) = expr {
            assert_eq!(parameters.len(), tt.expected.len());
            for (i, param) in parameters.iter().enumerate() {
                assert_eq!(param.as_string(), tt.expected[i]);
            }
        } else {
            panic!("Expected Expression statement");
        }
    }
}

#[test]
fn test_call_expression() {
    let input = "add(1, 2 * 3, 4 + 5)";
    let program = prepare_program_for_test(input);
    assert_eq!(program.statements.len(), 1);
    let stmt = unwrap_expression(&program.statements[0]);
    if let Expression::CallExpression(CallExpression {
        function,
        arguments,
        ..
    }) = stmt
    {
        test_identifier(function, "add");
        assert_eq!(arguments.len(), 3);
        test_literal_expression(&arguments[0], 1);
        test_infix_expression(&arguments[1], 2, "*", 3);
        test_infix_expression(&arguments[2], 4, "+", 5);
    } else {
        panic!("Not a call expression")
    }
}

macro_rules! test_object {
    ($result: expr, $expected: expr, $variant: ident) => {
        match $result {
            Object::$variant { value } => assert_eq!(value, $expected),
            _ => panic!("Expected {} object", stringify!($variant)),
        }
    };
}

fn test_evaluator(input: &str) -> Result<Object, EvalError> {
    let program = prepare_program_for_test(input);
    let env = Environment::new();
    program.evaluate(&env)
}

#[test]
fn test_eval_integer_expression() {
    struct IntEvalTest {
        input: &'static str,
        expected: i64,
    }
    let tests = [
        IntEvalTest {
            input: "5",
            expected: 5,
        },
        IntEvalTest {
            input: "10",
            expected: 10,
        },
        IntEvalTest {
            input: "-5",
            expected: -5,
        },
        IntEvalTest {
            input: "-10",
            expected: -10,
        },
        IntEvalTest {
            input: "5 + 5 + 5 + 5 - 10",
            expected: 10,
        },
        IntEvalTest {
            input: "2 * 2 * 2 * 2 * 2",
            expected: 32,
        },
        IntEvalTest {
            input: "-50 + 100 + -50",
            expected: 0,
        },
        IntEvalTest {
            input: "5 * 2 + 10",
            expected: 20,
        },
        IntEvalTest {
            input: "5 + 2 * 10",
            expected: 25,
        },
        IntEvalTest {
            input: "20 + 2 * -10",
            expected: 0,
        },
        IntEvalTest {
            input: "50 / 2 * 2 + 10",
            expected: 60,
        },
        IntEvalTest {
            input: "2 * (5 + 10)",
            expected: 30,
        },
        IntEvalTest {
            input: "3 * 3 * 3 + 10",
            expected: 37,
        },
        IntEvalTest {
            input: "3 * (3 * 3) + 10",
            expected: 37,
        },
        IntEvalTest {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: 50,
        },
    ];

    for tt in tests {
        let evaluated = test_evaluator(tt.input).unwrap();
        test_object!(evaluated, tt.expected, Integer);
    }
}

#[test]
fn test_boolean_expression() {
    struct BoolEvalTest {
        input: &'static str,
        expected: bool,
    }

    let tests = [
        BoolEvalTest {
            input: "true",
            expected: true,
        },
        BoolEvalTest {
            input: "false",
            expected: false,
        },
        BoolEvalTest {
            input: "1 < 2",
            expected: true,
        },
        BoolEvalTest {
            input: "1 > 2",
            expected: false,
        },
        BoolEvalTest {
            input: "1 < 1",
            expected: false,
        },
        BoolEvalTest {
            input: "1 > 1",
            expected: false,
        },
        BoolEvalTest {
            input: "1 == 1",
            expected: true,
        },
        BoolEvalTest {
            input: "1 != 1",
            expected: false,
        },
        BoolEvalTest {
            input: "1 == 2",
            expected: false,
        },
        BoolEvalTest {
            input: "1 != 2",
            expected: true,
        },
        BoolEvalTest {
            input: "true == true",
            expected: true,
        },
        BoolEvalTest {
            input: "false == false",
            expected: true,
        },
        BoolEvalTest {
            input: "true == false",
            expected: false,
        },
        BoolEvalTest {
            input: "true != false",
            expected: true,
        },
        BoolEvalTest {
            input: "false != true",
            expected: true,
        },
        BoolEvalTest {
            input: "(1 < 2) == true",
            expected: true,
        },
        BoolEvalTest {
            input: "(1 < 2) == false",
            expected: false,
        },
        BoolEvalTest {
            input: "(1 > 2) == true",
            expected: false,
        },
        BoolEvalTest {
            input: "(1 > 2) == false",
            expected: true,
        },
    ];

    for tt in tests {
        let evaluated = test_evaluator(tt.input).unwrap();
        test_object!(evaluated, tt.expected, Boolean);
    }
}

#[test]
fn test_bang_operator() {
    struct BangTest {
        input: &'static str,
        expected: bool,
    }

    let tests = [
        BangTest {
            input: "!true",
            expected: false,
        },
        BangTest {
            input: "!false",
            expected: true,
        },
        BangTest {
            input: "!5",
            expected: false,
        },
        BangTest {
            input: "!!true",
            expected: true,
        },
        BangTest {
            input: "!!false",
            expected: false,
        },
        BangTest {
            input: "!!5",
            expected: true,
        },
    ];

    for tt in tests {
        let evaluated = test_evaluator(tt.input).unwrap();
        test_object!(evaluated, tt.expected, Boolean)
    }
}

#[test]
fn test_evaluate_if_else_expressions() {
    struct IfElseTest {
        input: &'static str,
        expected: Object,
    }
    let tests = [
        IfElseTest {
            input: "if (true) { 10 }",
            expected: Object::Integer { value: 10 },
        },
        IfElseTest {
            input: "if (false) { 10 }",
            expected: Object::Null,
        },
        IfElseTest {
            input: "if (1) { 10 }",
            expected: Object::Integer { value: 10 },
        },
        IfElseTest {
            input: "if (1 < 2) { 10 }",
            expected: Object::Integer { value: 10 },
        },
        IfElseTest {
            input: "if (1 > 2) { 10 }",
            expected: Object::Null,
        },
        IfElseTest {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: Object::Integer { value: 10 },
        },
        IfElseTest {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: Object::Integer { value: 20 },
        },
    ];

    for tt in tests {
        let evaluated = test_evaluator(tt.input).unwrap();
        match evaluated {
            Object::Integer { .. } => {
                if let Object::Integer { value } = tt.expected {
                    test_object!(evaluated, value, Integer)
                } else {
                    unreachable!()
                }
            }
            Object::Null => assert_eq!(evaluated, object::NULL),
            _ => unreachable!(),
        }
    }
}

#[test]
fn test_return_statement_evaluation() {
    struct ReturnTest {
        input: &'static str,
        expected: i64,
    }

    let tests = [
        ReturnTest {
            input: "return 10",
            expected: 10,
        },
        ReturnTest {
            input: "return 10; 9;",
            expected: 10,
        },
        ReturnTest {
            input: "return 2 * 5; 9;",
            expected: 10,
        },
        ReturnTest {
            input: "9; return 2 * 5; 9;",
            expected: 10,
        },
        ReturnTest {
            input: "if (10 > 1) { if (10 > 1) { return 10 ;} return 1; } ",
            expected: 10,
        },
    ];

    for tt in tests {
        let evaluated = test_evaluator(tt.input).unwrap();
        test_object!(evaluated, tt.expected, Integer);
    }
}

#[test]
fn test_error_handling() {
    struct ErrorHandlingTest {
        input: &'static str,
        expected: &'static str,
    }

    let tests = [
        ErrorHandlingTest {
            input: "5 + true",
            expected: "type mismatch: INTEGER + BOOLEAN",
        },
        ErrorHandlingTest {
            input: "5 + true; 5",
            expected: "type mismatch: INTEGER + BOOLEAN",
        },
        ErrorHandlingTest {
            input: "-true",
            expected: "unkown operator: -BOOLEAN",
        },
        ErrorHandlingTest {
            input: "true + true",
            expected: "unkown operator: BOOLEAN + BOOLEAN",
        },
        ErrorHandlingTest {
            input: "5; true + false; 5;",
            expected: "unkown operator: BOOLEAN + BOOLEAN",
        },
        ErrorHandlingTest {
            input: "if (10 > 1) { true + false }",
            expected: "unkown operator: BOOLEAN + BOOLEAN",
        },
        ErrorHandlingTest {
            input: "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
            expected: "unkown operator: BOOLEAN + BOOLEAN",
        },
        ErrorHandlingTest {
            input: "foobar;",
            expected: "identifier not found: foobar",
        },
        ErrorHandlingTest {
            input: "\"Hello\" - \"World\"",
            expected: "unknown operator: STRING - STRING",
        },
        ErrorHandlingTest {
            input: "{\"name\": \"monkey\"}[fn(x) {x}]",
            expected: "unusable as hash key: FUNCTION",
        },
    ];

    for tt in tests {
        let evaluated = test_evaluator(tt.input);
        if let Err(EvalError::Error { message }) | Err(EvalError::Unhashable { message }) =
            evaluated
        {
            assert_eq!(message, tt.expected);
        } else {
            panic!("Did not throw expected error: {}", tt.expected)
        }
    }
}

#[test]
fn test_let_statement_evaluation() {
    struct LetTest {
        input: &'static str,
        expected: i64,
    }

    let tests = [
        LetTest {
            input: "let a = 5; a;",
            expected: 5,
        },
        LetTest {
            input: "let a = 5 * 5; a;",
            expected: 25,
        },
        LetTest {
            input: "let a = 5; let b = a; b;",
            expected: 5,
        },
        LetTest {
            input: "let a = 5; let b = a; let c = b + a + 5; c;",
            expected: 15,
        },
    ];

    for tt in tests {
        test_object!(test_evaluator(tt.input).unwrap(), tt.expected, Integer);
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; };";
    let evaluated = test_evaluator(input);
    if let Ok(Object::Function {
        parameters, body, ..
    }) = evaluated
    {
        assert_eq!(parameters.len(), 1);
        assert_eq!(parameters[0].as_string(), "x");
        assert_eq!(body.to_string(), "{\n (x + 2)\n}");
    } else {
        panic!("This should be a function and not error, really")
    }
}

#[test]
fn test_function_application() {
    struct TestFnApplication {
        input: &'static str,
        expected: i64,
    }

    let tests = [
        TestFnApplication {
            input: "let identity = fn(x) { x; }; identity(5);",
            expected: 5,
        },
        TestFnApplication {
            input: "let identity = fn(x) {return x; }; identity(5);",
            expected: 5,
        },
        TestFnApplication {
            input: "let double = fn(x) { 2 * x; }; double(5);",
            expected: 10,
        },
        TestFnApplication {
            input: "let add = fn(x, y) {x + y; }; add(5, 5);",
            expected: 10,
        },
        TestFnApplication {
            input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            expected: 20,
        },
        TestFnApplication {
            input: "let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2); addTwo(2);",
            expected: 4,
        },
    ];

    for tt in tests {
        test_object!(test_evaluator(tt.input).unwrap(), tt.expected, Integer);
    }
}

#[test]
fn test_string_literal_expression() {
    let input = "\"hello world\"";
    let program = prepare_program_for_test(input);
    assert_eq!(program.statements.len(), 1);
    if let Statement::Expression {
        value: Expression::String(StringLiteral { value, .. }),
        ..
    } = &program.statements[0]
    {
        assert_eq!(value, "hello world");
    } else {
        panic!("Is not a string");
    }
}

#[test]
fn test_string_literal_evaluation() {
    let input = "\"hello world\"";
    let evaluated = test_evaluator(input);
    if let Ok(Object::String { value }) = evaluated {
        assert_eq!(value, "hello world")
    } else {
        panic!("Did not evaluate to string or errored")
    }
}

#[test]
fn test_string_concatenation() {
    let input = "\"Hello\" + \" \" + \"World\"";
    let evaluated = test_evaluator(input);
    if let Ok(Object::String { value }) = evaluated {
        assert_eq!(value, "Hello World");
    } else {
        panic!("Is not a String object");
    }
}

#[test]
fn test_builtin_functions() {
    enum Expected<'a> {
        Len(i64),
        Null(Object),
        IntArray(Vec<i64>),
        Error(&'a str),
    }
    struct TestBuiltIns<'a> {
        input: &'static str,
        expected: Expected<'a>,
    }

    let tests = [
        TestBuiltIns {
            input: "len(\"\")",
            expected: Expected::Len(0),
        },
        TestBuiltIns {
            input: "len(\"four\")",
            expected: Expected::Len(4),
        },
        TestBuiltIns {
            input: "len(\"hello world\")",
            expected: Expected::Len(11),
        },
        TestBuiltIns {
            input: "len(1)",
            expected: Expected::Error("argument to `len` not supported, got INTEGER"),
        },
        TestBuiltIns {
            input: "len(\"one\", \"two\")",
            expected: Expected::Error("wrong number of arguments. got=2, want=1"),
        },
        TestBuiltIns {
            input: "len([1, 2, 3])",
            expected: Expected::Len(3),
        },
        TestBuiltIns {
            input: "let myArr = [1, 2, 3]; len(myArr)",
            expected: Expected::Len(3),
        },
        TestBuiltIns {
            input: "first([1, 2, 3])",
            expected: Expected::Len(1),
        },
        TestBuiltIns {
            input: "first([])",
            expected: Expected::Null(NULL),
        },
        TestBuiltIns {
            input: "last([1, 2, 3])",
            expected: Expected::Len(3),
        },
        TestBuiltIns {
            input: "last([])",
            expected: Expected::Null(NULL),
        },
        TestBuiltIns {
            input: "rest([1, 2, 3])",
            expected: Expected::IntArray(vec![2, 3]),
        },
        TestBuiltIns {
            input: "rest([])",
            expected: Expected::Null(NULL),
        },
        TestBuiltIns {
            input: "push([], 1)",
            expected: Expected::IntArray(vec![1]),
        },
        TestBuiltIns {
            input: "push([1, 2, 3], 4)",
            expected: Expected::IntArray(vec![1, 2, 3, 4]),
        },
    ];

    for tt in tests {
        let evaluated = test_evaluator(tt.input);
        match tt.expected {
            Expected::Len(expect) => {
                if let Ok(result_object) = evaluated {
                    test_object!(result_object, expect, Integer)
                } else {
                    panic!("Builtin evaluation errored. Got {:?}", evaluated)
                }
            }
            Expected::Null(neither) => {
                if let Ok(result) = evaluated {
                    assert_eq!(neither, result)
                } else {
                    panic!("Builtin errored when expecting null value")
                }
            }
            Expected::IntArray(arr) => {
                if let Ok(Object::Array { elements }) = evaluated {
                    assert_eq!(
                        arr,
                        elements
                            .iter()
                            .map(|i| {
                                if let Object::Integer { value } = i {
                                    *value
                                } else {
                                    panic!("Expected integer elements")
                                }
                            })
                            .collect::<Vec<i64>>()
                    )
                } else {
                    panic!("Expected Array")
                }
            }
            Expected::Error(err) => {
                if let Err(erroneous) = evaluated {
                    if let EvalError::Error { message } = erroneous {
                        assert_eq!(err, message)
                    } else {
                        panic!("Got a different error")
                    };
                } else {
                    panic!("Expected error")
                }
            }
        }
    }
}

#[test]
fn test_parse_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let program = prepare_program_for_test(input);
    assert_eq!(program.statements.len(), 1);

    if let Statement::Expression {
        value: Expression::Array(ArrayLiteral { elements, .. }),
        ..
    } = &program.statements[0]
    {
        assert_eq!(elements.len(), 3);
        test_integer_literal(&elements[0], 1);
        test_infix_expression(&elements[1], 2, "*", 2);
        test_infix_expression(&elements[2], 3, "+", 3);
    } else {
        panic!("Did not get array");
    }
}

#[test]
fn test_parse_index_expression() {
    let input = "myArray[1 + 1]";
    let program = prepare_program_for_test(input);
    assert_eq!(program.statements.len(), 1);
    if let Statement::Expression {
        value: Expression::Index(idx),
        ..
    } = &program.statements[0]
    {
        test_identifier(&idx.left, "myArray");
        test_infix_expression(&idx.index, 1, "+", 1);
    } else {
        panic!("did not find index expression")
    }
}

#[test]
fn test_array_literal() {
    let input = "[1, 2 * 2, 3 + 3]";
    let evaluated = test_evaluator(input);
    if let Ok(Object::Array { elements }) = evaluated {
        assert_eq!(elements.len(), 3);
        test_object!(elements[0], 1, Integer);
        test_object!(elements[1], 4, Integer);
        test_object!(elements[2], 6, Integer);
    } else {
        panic!("Did not get array object")
    }
}

#[test]
fn test_array_index_expressions() {
    struct ArrayIndexExpressionTest {
        input: &'static str,
        expected: Result<i64, EvalError>,
    }

    let tests = [
        ArrayIndexExpressionTest {
            input: "[1, 2, 3][0]",
            expected: Ok(1),
        },
        ArrayIndexExpressionTest {
            input: "[1, 2, 3][1]",
            expected: Ok(2),
        },
        ArrayIndexExpressionTest {
            input: "[1, 2, 3][2]",
            expected: Ok(3),
        },
        ArrayIndexExpressionTest {
            input: "let i = 0; [1, 2, 3][i]",
            expected: Ok(1),
        },
        ArrayIndexExpressionTest {
            input: "[1, 2, 3][1 + 1]",
            expected: Ok(3),
        },
        ArrayIndexExpressionTest {
            input: "[1, 2, 3][3]",
            expected: Err(EvalError::IndexError {
                message: "index, 3, out of bounds for array of length 3".to_string(),
            }),
        },
        ArrayIndexExpressionTest {
            input: "[1, 2, 3][-1]",
            expected: Err(EvalError::IndexError {
                message: "index must not be negative".to_string(),
            }),
        },
        ArrayIndexExpressionTest {
            input: "let myArray = [1, 2, 3]; myArray[2];",
            expected: Ok(3),
        },
        ArrayIndexExpressionTest {
            input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            expected: Ok(6),
        },
        ArrayIndexExpressionTest {
            input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
            expected: Ok(2),
        },
    ];

    for tt in tests {
        let evaluated = test_evaluator(tt.input);
        match tt.expected {
            Ok(expected) => {
                test_object!(evaluated.unwrap(), expected, Integer);
            }
            Err(err) => {
                assert_eq!(err, evaluated.unwrap_err());
            }
        }
    }
}

#[test]
fn test_parse_hash_literal_string_keys() {
    let input = "{\"one\": 1, \"two\": 2, \"three\": 3,}";
    let expected = [("one", 1), ("two", 2), ("three", 3)];
    let program = prepare_program_for_test(input);
    assert_eq!(program.statements.len(), 1);
    if let Statement::Expression {
        value: Expression::HashMap(HashLiteral { pairs, .. }),
        ..
    } = &program.statements[0]
    {
        assert_eq!(pairs.len(), 3);
        for (idx, value) in pairs.iter().enumerate() {
            assert_eq!(
                value.0,
                Expression::from_string(expected[idx].0.to_string())
            );

            assert_eq!(value.1, Expression::from_int(expected[idx].1));
        }
    } else {
        panic!("Did not parse hashmap")
    }
}

#[test]
fn test_parse_hash_literal_bool_keys() {
    let input = "{true: 1, false: 2}";
    let expected = [(true, 1), (false, 2)];
    let program = prepare_program_for_test(input);
    assert_eq!(program.statements.len(), 1);
    if let Statement::Expression {
        value: Expression::HashMap(HashLiteral { pairs, .. }),
        ..
    } = &program.statements[0]
    {
        assert_eq!(pairs.len(), 2);
        for (idx, value) in pairs.iter().enumerate() {
            assert_eq!(value.0, Expression::from_bool(expected[idx].0));

            assert_eq!(value.1, Expression::from_int(expected[idx].1));
        }
    } else {
        panic!("Did not parse hashmap")
    }
}

#[test]
fn test_parse_hash_literal_int_keys() {
    let input = "{1: 1, 2: 2, 3: 3}";
    let expected = [(1, 1), (2, 2), (3, 3)];
    let program = prepare_program_for_test(input);
    assert_eq!(program.statements.len(), 1);
    if let Statement::Expression {
        value: Expression::HashMap(HashLiteral { pairs, .. }),
        ..
    } = &program.statements[0]
    {
        assert_eq!(pairs.len(), 3);
        for (idx, value) in pairs.iter().enumerate() {
            assert_eq!(value.0, Expression::from_int(expected[idx].0));

            assert_eq!(value.1, Expression::from_int(expected[idx].1));
        }
    } else {
        panic!("Did not parse hashmap")
    }
}

#[test]
fn test_parse_hash_literal_expression_keys() {
    let input = "{1: 0 + 1, 2: 10 - 8, 3: 15 / 5}";
    let program = prepare_program_for_test(input);
    assert_eq!(program.statements.len(), 1);
    if let Statement::Expression {
        value: Expression::HashMap(HashLiteral { pairs, .. }),
        ..
    } = &program.statements[0]
    {
        assert_eq!(pairs.len(), 3);

        assert_eq!(pairs[0].0, Expression::from_int(1));
        assert!(test_infix_expression(&pairs[0].1, 0, "+", 1));

        assert_eq!(pairs[1].0, Expression::from_int(2));
        assert!(test_infix_expression(&pairs[1].1, 10, "-", 8));

        assert_eq!(pairs[2].0, Expression::from_int(3));
        assert!(test_infix_expression(&pairs[2].1, 15, "/", 5));
    } else {
        panic!("Did not parse hashmap")
    }
}

#[test]
fn test_parse_empty_hash_literal() {
    let input = "{}";
    let program = prepare_program_for_test(input);
    assert_eq!(program.statements.len(), 1);
    if let Statement::Expression {
        value: Expression::HashMap(HashLiteral { pairs, .. }),
        ..
    } = &program.statements[0]
    {
        assert_eq!(pairs.len(), 0)
    } else {
        panic!("Did not get (empty) hashmap");
    }
}

#[test]
fn test_string_hash_key() {
    let hello1 = Object::String {
        value: "Hello World".to_string(),
    };
    let hello2 = Object::String {
        value: "Hello World".to_string(),
    };

    let diff1 = Object::String {
        value: "My name is Johnny".to_string(),
    };
    let diff2 = Object::String {
        value: "My name is Johnny".to_string(),
    };

    if hello1.hash().unwrap() != hello2.hash().unwrap() {
        panic!("string with same content has different hash key");
    }

    if diff1.hash().unwrap() != diff2.hash().unwrap() {
        panic!("string with same content has different hash key");
    }

    if diff1.hash().unwrap() == hello1.hash().unwrap() {
        panic!("string with different content has same hash key");
    }
}

#[test]
fn test_int_hash_key() {
    let val1 = Object::Integer { value: 1 };
    let val1_ = Object::Integer { value: 1 };
    let val2 = Object::Integer { value: 2 };

    if val1.hash().unwrap() != val1_.hash().unwrap() {
        panic!("int with same content has different hash key");
    }

    if val2.hash().unwrap() == val1.hash().unwrap() {
        panic!("int with different content has same hash key");
    }
}

#[test]
fn test_bool_hash_key() {
    let yes = Object::Boolean { value: true };
    let yep = Object::Boolean { value: true };
    let nope = Object::Boolean { value: false };

    if yes.hash().unwrap() != yep.hash().unwrap() {
        panic!("bool with same content has different hash key");
    }

    if yep.hash().unwrap() == nope.hash().unwrap() {
        panic!("bool with different content has same hash key");
    }
}

#[test]
fn test_hash_literal() {
    let input = "let two = \"two\"; 
        {
        \"one\": 10 -9, two : 1 + 1, \"thr\" + \"ee\":6 / 2,
        4: 4, true: 5, false: 6
        }
        ";
    let evaluated = test_evaluator(input);
    let expected = vec![
        (
            Object::String {
                value: "one".to_string(),
            }
            .hash()
            .unwrap(),
            1,
        ),
        (
            Object::String {
                value: "two".to_string(),
            }
            .hash()
            .unwrap(),
            2,
        ),
        (
            Object::String {
                value: "three".to_string(),
            }
            .hash()
            .unwrap(),
            3,
        ),
        (Object::Integer { value: 4 }.hash().unwrap(), 4),
        (TRUE.hash().unwrap(), 5),
        (FALSE.hash().unwrap(), 6),
    ];
    if let Ok(Object::Hash { pairs }) = evaluated {
        assert_eq!(pairs.len(), expected.len());
        for (ex_key, ex_value) in expected {
            let key_val = pairs.get(&ex_key).unwrap();
            test_object!(key_val.value, ex_value, Integer);
        }
    } else {
        panic!("Expected Hashmap")
    }
}
#[test]
fn test_hash_index_expressions() {
    enum Expected {
        Integer(i64),
        Null,
    }

    struct HashIndexTest {
        input: &'static str,
        expected: Expected,
    }

    let tests = [
        HashIndexTest {
            input: r#"{"foo": 5}["foo"]"#,
            expected: Expected::Integer(5),
        },
        HashIndexTest {
            input: r#"{"foo": 5}["bar"]"#,
            expected: Expected::Null,
        },
        HashIndexTest {
            input: r#"let key = "foo"; {"foo": 5}[key]"#,
            expected: Expected::Integer(5),
        },
        HashIndexTest {
            input: r#"{}["foo"]"#,
            expected: Expected::Null,
        },
        HashIndexTest {
            input: "{5: 5}[5]",
            expected: Expected::Integer(5),
        },
        HashIndexTest {
            input: "{true: 5}[true]",
            expected: Expected::Integer(5),
        },
        HashIndexTest {
            input: "{false: 5}[false]",
            expected: Expected::Integer(5),
        },
    ];

    for tt in tests {
        let evaluated = test_evaluator(tt.input).unwrap();
        match tt.expected {
            Expected::Integer(expected_val) => {
                test_object!(evaluated, expected_val, Integer);
            }
            Expected::Null => {
                assert_eq!(evaluated, NULL);
            }
        }
    }
}
