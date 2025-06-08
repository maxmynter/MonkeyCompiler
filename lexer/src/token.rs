use lazy_static::lazy_static;
use std::collections::HashMap;
use std::rc::Rc;

#[allow(clippy::upper_case_acronyms)]
#[derive(Copy, Hash, Debug, Eq, PartialEq, Clone)]
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
    COLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
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
            ('[', TokenType::LBRACKET),
            (']', TokenType::RBRACKET),
            (':', TokenType::COLON),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenType,
    pub literal: Rc<String>,
}

impl Token {
    pub fn new(kind: TokenType, literal: &str) -> Self {
        Token {
            kind,
            literal: literal.to_string().into(),
        }
    }
    pub fn empty() -> Self {
        Token {
            kind: TokenType::EOF,
            literal: "\0".to_string().into(),
        }
    }
}
