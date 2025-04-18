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
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
}

lazy_static! {
    static ref ATOMS: HashMap<char, TokenType> = {
        [
            ('=', TokenType::ASSIGN),
            ('+', TokenType::PLUS),
            ('(', TokenType::LPAREN),
            (')', TokenType::RPAREN),
            ('{', TokenType::LBRACE),
            ('}', TokenType::RBRACE),
            (';', TokenType::SEMICOLON),
            (',', TokenType::COMMA),
        ]
        .iter()
        .cloned()
        .collect()
    };
}

struct Keywords {}
impl Keywords {
    fn get(literal: &str) -> Option<TokenType> {
        match literal {
            "fn" => Some(TokenType::FUNCTION),
            "let" => Some(TokenType::LET),
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

    fn read_molecule<F>(&mut self, predicate: F) -> &str
    where
        F: Fn(char) -> bool,
    {
        let position = self.position;
        while predicate(self.ch) {
            self.eat_symbol();
        }
        &self.input[position..self.position]
    }
    fn read_identifier(&mut self) -> &str {
        self.read_molecule(Lexer::is_letter)
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

    fn read_number(&mut self) -> &str {
        self.read_molecule(Lexer::is_digit)
    }

    fn read_symbol(&mut self) -> &str {
        let literal_start = self.position;
        let literal_end = self.position + self.ch.len_utf8();
        self.eat_symbol();
        &self.input[literal_start..literal_end]
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            '\0' => Token {
                kind: TokenType::EOF,
                literal: "",
            },
            ch if ATOMS.contains_key(&ch) => {
                let symbol = self.read_symbol();
                Token {
                    kind: ATOMS[&ch].clone(),
                    literal: symbol,
                }
            }
            ch if Lexer::is_letter(ch) => {
                let identifier = self.read_identifier();
                Token {
                    kind: Keywords::lookup_ident(identifier),
                    literal: identifier,
                }
            }
            ch if Lexer::is_digit(ch) => {
                let number = self.read_number();
                Token {
                    kind: TokenType::INT,
                    literal: number,
                }
            }
            _ch => {
                let position = self.position;
                self.eat_symbol();
                Token {
                    kind: TokenType::ILLEGAL,
                    literal: &self.input[position..self.position],
                }
            }
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
        "let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);",
    );
    let expected = vec![
        // let five = 5;
        Token {
            kind: TokenType::LET,
            literal: "let",
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
            literal: "let",
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
            literal: "let",
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
            literal: "fn",
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
            literal: "let",
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
        // EOF
        Token {
            kind: TokenType::EOF,
            literal: "",
        },
    ];
    let mut lexer = Lexer::new(&input);
    for i in 0..expected.len() {
        let tok = lexer.next_token();
        println!("{:?}-:-{:?}", &tok, &expected[i]);
        assert_eq!(tok, expected[i])
    }
}
