use core::str;
use std::str;

enum TokenType {
    STRING,
    INT,
    IDENT,
    ILLEGAL,
    EOF,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
}

struct Token<'a> {
    kind: TokenType,
    literal: &'a str,
}

struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
}
impl Lexer {
    fn ch(&self) -> u8 {
        if self.read_position > self.input.len() {
            return 0;
        }
        self.input[self.read_position]
    }
}

impl Lexer {
    fn new(input: String) -> Lexer {
        Lexer {
            input: input.into_bytes(),
            position: 0,
            read_position: 0,
        }
    }
    fn read_char(&mut self) {
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn eat(&mut self) -> Token {
        todo!()
    }
}

#[test]
fn test_eat_token() {
    let input = String::from("=+(){},;");
    let expected = vec![
        Token {
            kind: TokenType::ASSIGN,
            literal: "=",
        },
        Token {
            kind: TokenType::PLUS,
            literal: "+",
        },
        Token {
            kind: TokenType::LPAREN,
            literal: "(",
        },
        Token {
            kind: TokenType::RPAREN,
            literal: ")",
        },
        Token {
            kind: TokenType::LBRACE,
            literal: "{",
        },
        Token {
            kind: TokenType::RBRACE,
            literal: "}",
        },
    ];
    let lexer = Lexer::new(input);
    for (i, tt) in input.iter().enumerate() {
        let tok = eat();
        assert_eq!(tok, expected[i], "Token did not equal target")
    }
}
