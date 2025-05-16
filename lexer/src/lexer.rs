pub mod token;
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
            Token::new(TWO_CHAR_ATOMS[two_char].clone(), two_char)
        } else {
            let current_char = self.ch;
            self.eat_symbol();
            let one_char = &self.input[start_pos..self.position];
            Token::new(ATOMS[&current_char].clone(), one_char)
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
            literal: number.to_string().into(),
        }
    }

    fn read_illegal(&mut self) -> Token {
        let position = self.position;
        self.eat_symbol();
        Token {
            kind: TokenType::ILLEGAL,
            literal: self.input[position..self.position].to_string().into(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        match self.ch {
            '\0' => Token {
                kind: TokenType::EOF,
                literal: "".to_string().into(),
            },
            ch if ATOMS.contains_key(&ch) => self.read_atom(),
            ch if Lexer::is_letter(ch) => self.read_identifier(),
            ch if Lexer::is_digit(ch) => self.read_number(),
            _ch => self.read_illegal(),
        }
    }
}

#[test]
fn test_next_token() {
    let input = String::from("=+(){},;");
    let expected = vec![
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
