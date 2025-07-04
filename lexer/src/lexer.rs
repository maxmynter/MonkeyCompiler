pub mod token;
use std::rc::Rc;
pub use token::{Keywords, Token, TokenType};
use token::{ATOMS, TWO_CHAR_ATOMS};

pub struct Lexer<'a> {
    input: &'a str,
    ch: char,
    position: usize,
    read_position: usize,
}
impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            input,
            ch: '\0',
            position: 0,
            read_position: 0,
        };
        lexer.eat_symbol();
        lexer
    }

    fn read_atom(&mut self) -> Token {
        let start_pos = self.position;
        if (self.ch == '=' || self.ch == '!') && self.peek_symbol() == '=' {
            self.eat_symbol();
            self.eat_symbol();
            let two_char = &self.input[start_pos..self.position];
            Token::new(TWO_CHAR_ATOMS[two_char], two_char)
        } else {
            let current_char = self.ch;
            self.eat_symbol();
            let one_char = &self.input[start_pos..self.position];
            Token::new(ATOMS[&current_char], one_char)
        }
    }

    fn eat_symbol(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position..]
                .chars()
                .next()
                .unwrap_or('\0')
        }
        self.position = self.read_position;
        self.read_position += if self.ch == '\0' {
            0
        } else {
            self.ch.len_utf8()
        };
    }

    fn peek_symbol(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position..].chars().next().unwrap()
        }
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn eat_molecule<F>(&mut self, predicate: F) -> &str
    where
        F: Fn(char) -> bool,
    {
        let position = self.position;
        while predicate(self.ch) {
            self.eat_symbol();
        }
        &self.input[position..self.position]
    }
    fn read_identifier(&mut self) -> Token {
        let identifier = self.eat_molecule(Lexer::is_letter);
        Token::new(Keywords::lookup_ident(identifier), identifier)
    }

    fn is_whitespace(ch: char) -> bool {
        ch.is_whitespace()
    }

    fn skip_whitespace(&mut self) {
        while Lexer::is_whitespace(self.ch) {
            self.eat_symbol();
        }
    }

    fn is_digit(ch: char) -> bool {
        ch.is_ascii_digit() // Later use is_digit to support unicode
    }

    fn read_number(&mut self) -> Token {
        let number = self.eat_molecule(Lexer::is_digit);
        Token {
            kind: TokenType::INT,
            literal: Rc::new(number.to_string()),
        }
    }

    fn read_illegal(&mut self) -> Token {
        let position = self.position;
        self.eat_symbol();
        Token {
            kind: TokenType::ILLEGAL,
            literal: Rc::new(self.input[position..self.position].to_string()),
        }
    }

    fn read_string(&mut self) -> Token {
        self.eat_symbol(); // opening quote
        let literal = Rc::new(self.eat_molecule(|ch| ch != '"' && ch != '\0').to_string());
        self.eat_symbol(); // closing quote
        Token {
            kind: TokenType::STRING,
            literal,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        match self.ch {
            '\0' => Token {
                kind: TokenType::EOF,
                literal: Rc::from(String::new()),
            },
            ch if ATOMS.contains_key(&ch) => self.read_atom(),
            ch if Lexer::is_letter(ch) => self.read_identifier(),
            '"' => self.read_string(),
            ch if Lexer::is_digit(ch) => self.read_number(),
            _ch => self.read_illegal(),
        }
    }
}
