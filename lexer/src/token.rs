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

struct Lexer<'a> {
    input: &'a str::Bytes,
    position: usize,
    read_position: usize,
    ch: &'a u8,
}

impl Lexer<'_> {
    fn new(input: String) -> Lexer {
        Lexer {
            input: &input.bytes(),
            position: 0,
            read_position: 0,
            ch: &input[0],
        }
    }
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = &self.input[self.read_position];
        }
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
