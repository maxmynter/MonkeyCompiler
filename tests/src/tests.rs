use ast::{
    CallExpression, Expression, FunctionLiteral, Identifier, IfExpression, IntegerLiteral, Node,
    Program, Statement,
};
use lexer::Lexer;
use lexer::{Token, TokenType};
use object::{CoerceObject, Environment, EvalError, NULL, Object, ObjectTraits};
use parser::Parser;
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
        let program = prepare_program_for_test(&tt.input);
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
    program.coerce(env.clone())
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
        let evaluated = test_evaluator(&tt.input).unwrap();
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
        let evaluated = test_evaluator(&tt.input).unwrap();
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
    ];

    for tt in tests {
        let evaluated = test_evaluator(tt.input);
        if let Err(EvalError::Error { message }) = evaluated {
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
        test_object!(test_evaluator(&tt.input).unwrap(), tt.expected, Integer);
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
        assert_eq!(body.to_string(), "(x + 2)");
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
        test_object!(test_evaluator(&tt.input).unwrap(), tt.expected, Integer);
    }
}
