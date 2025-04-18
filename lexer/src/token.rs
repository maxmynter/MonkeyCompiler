use lazy_static::lazy_static;
use std::collections::HashMap;

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TokenType {
    STRING,
    INT,
    IDENT,
    ILLEGAL,
    EOF,
    ASSIGN,
    PLUS,
    MINUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    UNEQ,
    FUNCTION,
    LET,
    IF,
    ELSE,
    RETURN,
    TRUE,
    FALSE,
}

lazy_static! {
    pub static ref ATOMS: HashMap<char, TokenType> = {
        [
            ('=', TokenType::ASSIGN),
            ('+', TokenType::PLUS),
            ('-', TokenType::MINUS),
            ('(', TokenType::LPAREN),
            (')', TokenType::RPAREN),
            ('{', TokenType::LBRACE),
            ('}', TokenType::RBRACE),
            (';', TokenType::SEMICOLON),
            (',', TokenType::COMMA),
            ('!', TokenType::BANG),
            ('*', TokenType::ASTERISK),
            ('/', TokenType::SLASH),
            ('<', TokenType::LT),
            ('>', TokenType::GT),
        ]
        .iter()
        .cloned()
        .collect()
    };
}

lazy_static! {
    pub static ref TWO_CHAR_ATOMS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("==", TokenType::EQ);
        m.insert("!=", TokenType::UNEQ);
        m
    };
}

pub struct Keywords {}
impl Keywords {
    fn get(literal: &str) -> Option<TokenType> {
        match literal {
            "fn" => Some(TokenType::FUNCTION),
            "let" => Some(TokenType::LET),
            "if" => Some(TokenType::IF),
            "else" => Some(TokenType::ELSE),
            "return" => Some(TokenType::RETURN),
            "true" => Some(TokenType::TRUE),
            "false" => Some(TokenType::FALSE),
            _ => None,
        }
    }
    pub fn lookup_ident(ident: &str) -> TokenType {
        if let Some(ident) = Keywords::get(ident) {
            ident
        } else {
            TokenType::IDENT
        }
    }
}

enum ReadAtomResult<'a> {
    TwoChar(TokenType, &'a str),
    OneChar(TokenType, &'a str),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub literal: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenType, literal: &'a str) -> Self {
        Token { kind, literal }
    }
}
