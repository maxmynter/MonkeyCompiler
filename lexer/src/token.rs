use lazy_static::lazy_static;
use std::collections::HashMap;

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Eq, PartialEq, Clone)]
enum TokenType {
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
    static ref ATOMS: HashMap<char, TokenType> = {
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
    static ref TWO_CHAR_ATOMS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("==", TokenType::EQ);
        m.insert("!=", TokenType::UNEQ);
        m
    };
}

struct Keywords {}
impl Keywords {
    fn get(literal: &str) -> Option<TokenType> {
        match literal {
            "slay" => Some(TokenType::FUNCTION),
            "hit" => Some(TokenType::LET),
            "frfr" => Some(TokenType::IF),
            "sike" => Some(TokenType::ELSE),
            "yeet" => Some(TokenType::RETURN),
            "fax" => Some(TokenType::TRUE),
            "cap" => Some(TokenType::FALSE),
            _ => None,
        }
    }
    fn lookup_ident(ident: &str) -> TokenType {
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
struct Token<'a> {
    kind: TokenType,
    literal: &'a str,
}

struct Lexer<'a> {
    input: &'a str,
    ch: char,
    position: usize,
    read_position: usize,
}
impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer {
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
            Token {
                kind: TWO_CHAR_ATOMS[two_char].clone(),
                literal: two_char,
            }
        } else {
            let current_char = self.ch.clone();
            self.eat_symbol();
            let one_char = &self.input[start_pos..self.position];
            Token {
                kind: ATOMS[&current_char].clone(),
                literal: one_char,
            }
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
        Token {
            kind: Keywords::lookup_ident(identifier),
            literal: identifier,
        }
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
            literal: number,
        }
    }

    fn read_illegal(&mut self) -> Token {
        let position = self.position;
        self.eat_symbol();
        Token {
            kind: TokenType::ILLEGAL,
            literal: &self.input[position..self.position],
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            '\0' => Token {
                kind: TokenType::EOF,
                literal: "",
            },
            ch if ATOMS.contains_key(&ch) => self.read_atom(),
            ch if Lexer::is_letter(ch) => self.read_identifier(),
            ch if Lexer::is_digit(ch) => self.read_number(),
            _ch => self.read_illegal(),
        };

        token
    }
}

#[test]
fn test_next_token() {
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
        Token {
            kind: TokenType::COMMA,
            literal: ",",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
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
        "hit five = 5;
        hit ten = 10;
        hit add = slay(x, y) {
            x + y;
        };
        hit result = add(five, ten);

        !-/*5;
        5 < 10 > 5;

        frfr (5 < 10) {
        yeet fax;
        } sike {
        yeet cap;
        }

        10 == 10;
        10 != 9;
        ",
    );
    let expected = vec![
        // let five = 5;
        Token {
            kind: TokenType::LET,
            literal: "hit",
        },
        Token {
            kind: TokenType::IDENT,
            literal: "five",
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=",
        },
        Token {
            kind: TokenType::INT,
            literal: "5",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
        },
        // let ten = 10;
        Token {
            kind: TokenType::LET,
            literal: "hit",
        },
        Token {
            kind: TokenType::IDENT,
            literal: "ten",
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=",
        },
        Token {
            kind: TokenType::INT,
            literal: "10",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
        },
        // let add = fn(x, y) {
        Token {
            kind: TokenType::LET,
            literal: "hit",
        },
        Token {
            kind: TokenType::IDENT,
            literal: "add",
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=",
        },
        Token {
            kind: TokenType::FUNCTION,
            literal: "slay",
        },
        Token {
            kind: TokenType::LPAREN,
            literal: "(",
        },
        Token {
            kind: TokenType::IDENT,
            literal: "x",
        },
        Token {
            kind: TokenType::COMMA,
            literal: ",",
        },
        Token {
            kind: TokenType::IDENT,
            literal: "y",
        },
        Token {
            kind: TokenType::RPAREN,
            literal: ")",
        },
        Token {
            kind: TokenType::LBRACE,
            literal: "{",
        },
        // x + y;
        Token {
            kind: TokenType::IDENT,
            literal: "x",
        },
        Token {
            kind: TokenType::PLUS,
            literal: "+",
        },
        Token {
            kind: TokenType::IDENT,
            literal: "y",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
        },
        // };
        Token {
            kind: TokenType::RBRACE,
            literal: "}",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
        },
        // let result = add(five, ten);
        Token {
            kind: TokenType::LET,
            literal: "hit",
        },
        Token {
            kind: TokenType::IDENT,
            literal: "result",
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=",
        },
        Token {
            kind: TokenType::IDENT,
            literal: "add",
        },
        Token {
            kind: TokenType::LPAREN,
            literal: "(",
        },
        Token {
            kind: TokenType::IDENT,
            literal: "five",
        },
        Token {
            kind: TokenType::COMMA,
            literal: ",",
        },
        Token {
            kind: TokenType::IDENT,
            literal: "ten",
        },
        Token {
            kind: TokenType::RPAREN,
            literal: ")",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
        },
        // !-/*5;
        Token {
            kind: TokenType::BANG,
            literal: "!",
        },
        Token {
            kind: TokenType::MINUS,
            literal: "-",
        },
        Token {
            kind: TokenType::SLASH,
            literal: "/",
        },
        Token {
            kind: TokenType::ASTERISK,
            literal: "*",
        },
        Token {
            kind: TokenType::INT,
            literal: "5",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
        },
        // 5 < 10 > 5;
        Token {
            kind: TokenType::INT,
            literal: "5",
        },
        Token {
            kind: TokenType::LT,
            literal: "<",
        },
        Token {
            kind: TokenType::INT,
            literal: "10",
        },
        Token {
            kind: TokenType::GT,
            literal: ">",
        },
        Token {
            kind: TokenType::INT,
            literal: "5",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
        },
        // if (5 < 10) {
        Token {
            kind: TokenType::IF,
            literal: "frfr",
        },
        Token {
            kind: TokenType::LPAREN,
            literal: "(",
        },
        Token {
            kind: TokenType::INT,
            literal: "5",
        },
        Token {
            kind: TokenType::LT,
            literal: "<",
        },
        Token {
            kind: TokenType::INT,
            literal: "10",
        },
        Token {
            kind: TokenType::RPAREN,
            literal: ")",
        },
        Token {
            kind: TokenType::LBRACE,
            literal: "{",
        },
        // return true;
        Token {
            kind: TokenType::RETURN,
            literal: "yeet",
        },
        Token {
            kind: TokenType::TRUE,
            literal: "fax",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
        },
        // } else {
        Token {
            kind: TokenType::RBRACE,
            literal: "}",
        },
        Token {
            kind: TokenType::ELSE,
            literal: "sike",
        },
        Token {
            kind: TokenType::LBRACE,
            literal: "{",
        },
        // return false;
        Token {
            kind: TokenType::RETURN,
            literal: "yeet",
        },
        Token {
            kind: TokenType::FALSE,
            literal: "cap",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
        },
        // }
        Token {
            kind: TokenType::RBRACE,
            literal: "}",
        },
        // 10 == 10;
        Token {
            kind: TokenType::INT,
            literal: "10",
        },
        Token {
            kind: TokenType::EQ,
            literal: "==",
        },
        Token {
            kind: TokenType::INT,
            literal: "10",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
        },
        // 10 != 9;
        Token {
            kind: TokenType::INT,
            literal: "10",
        },
        Token {
            kind: TokenType::UNEQ,
            literal: "!=",
        },
        Token {
            kind: TokenType::INT,
            literal: "9",
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";",
        },
        // EOF
        Token {
            kind: TokenType::EOF,
            literal: "",
        },
    ];
    let mut lexer = Lexer::new(&input);
    for i in 0..expected.len() {
        let tok = lexer.next_token();
        println!("{:?} -|- {:?}", tok, expected[i]);
        assert_eq!(tok, expected[i]);
    }
}
