use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq)]
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
            return ident;
        } else {
            return TokenType::IDENT;
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Token {
    kind: TokenType,
    literal: String,
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
        self.input[self.position]
    }

    fn ch_str(&self) -> String {
        std::str::from_utf8(&[self.ch()])
            .expect("Could not read byte")
            .to_string()
    }

    fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input.into_bytes(),
            position: 0,
            read_position: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn is_letter(ch: u8) -> bool {
        b'a' <= ch && ch <= b'z' || b'A' <= ch && ch <= b'Z' || ch == b'_'
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position.clone();

        while Lexer::is_letter(self.ch()) {
            self.read_char();
        }
        std::str::from_utf8(&self.input[position..self.position])
            .expect("Could not parse")
            .to_string()
    }

    fn next_token(&mut self) -> Token {
        let token = match self.ch() {
            b'=' => Token {
                kind: TokenType::ASSIGN,
                literal: self.ch_str(),
            },
            b'+' => Token {
                kind: TokenType::PLUS,
                literal: self.ch_str(),
            },
            b'(' => Token {
                kind: TokenType::LPAREN,
                literal: self.ch_str(),
            },
            b')' => Token {
                kind: TokenType::RPAREN,
                literal: self.ch_str(),
            },
            b'{' => Token {
                kind: TokenType::LBRACE,
                literal: self.ch_str(),
            },
            b'}' => Token {
                kind: TokenType::RBRACE,
                literal: self.ch_str(),
            },
            0 => Token {
                kind: TokenType::EOF,
                literal: String::new(),
            },
            _ => {
                if Lexer::is_letter(self.ch()) {
                    let identifier = self.read_identifier();
                    Token {
                        kind: Keywords::lookup_ident(&identifier),
                        literal: identifier,
                    }
                } else {
                    Token {
                        kind: TokenType::ILLEGAL,
                        literal: self.ch_str(),
                    }
                }
            }
        };
        self.read_char();
        token
    }
}

#[test]
fn test_next_token() {
    let input = String::from("=+(){},;");
    let expected = vec![
        Token {
            kind: TokenType::ASSIGN,
            literal: "=".to_string(),
        },
        Token {
            kind: TokenType::PLUS,
            literal: "+".to_string(),
        },
        Token {
            kind: TokenType::LPAREN,
            literal: "(".to_string(),
        },
        Token {
            kind: TokenType::RPAREN,
            literal: ")".to_string(),
        },
        Token {
            kind: TokenType::LBRACE,
            literal: "{".to_string(),
        },
        Token {
            kind: TokenType::RBRACE,
            literal: "}".to_string(),
        },
    ];
    let mut lexer = Lexer::new(input);
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
            literal: "let".to_string(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "five".to_string(),
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=".to_string(),
        },
        Token {
            kind: TokenType::INT,
            literal: "5".to_string(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string(),
        },
        // let ten = 10;
        Token {
            kind: TokenType::LET,
            literal: "let".to_string(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "ten".to_string(),
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=".to_string(),
        },
        Token {
            kind: TokenType::INT,
            literal: "10".to_string(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string(),
        },
        // let add = fn(x, y) {
        Token {
            kind: TokenType::LET,
            literal: "let".to_string(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "add".to_string(),
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=".to_string(),
        },
        Token {
            kind: TokenType::FUNCTION,
            literal: "fn".to_string(),
        },
        Token {
            kind: TokenType::LPAREN,
            literal: "(".to_string(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "x".to_string(),
        },
        Token {
            kind: TokenType::COMMA,
            literal: ",".to_string(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "y".to_string(),
        },
        Token {
            kind: TokenType::RPAREN,
            literal: ")".to_string(),
        },
        Token {
            kind: TokenType::LBRACE,
            literal: "{".to_string(),
        },
        // x + y;
        Token {
            kind: TokenType::IDENT,
            literal: "x".to_string(),
        },
        Token {
            kind: TokenType::PLUS,
            literal: "+".to_string(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "y".to_string(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string(),
        },
        // };
        Token {
            kind: TokenType::RBRACE,
            literal: "}".to_string(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string(),
        },
        // let result = add(five, ten);
        Token {
            kind: TokenType::LET,
            literal: "let".to_string(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "result".to_string(),
        },
        Token {
            kind: TokenType::ASSIGN,
            literal: "=".to_string(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "add".to_string(),
        },
        Token {
            kind: TokenType::LPAREN,
            literal: "(".to_string(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "five".to_string(),
        },
        Token {
            kind: TokenType::COMMA,
            literal: ",".to_string(),
        },
        Token {
            kind: TokenType::IDENT,
            literal: "ten".to_string(),
        },
        Token {
            kind: TokenType::RPAREN,
            literal: ")".to_string(),
        },
        Token {
            kind: TokenType::SEMICOLON,
            literal: ";".to_string(),
        },
        // EOF
        Token {
            kind: TokenType::EOF,
            literal: "".to_string(),
        },
    ];
    let mut lexer = Lexer::new(input);
    for i in 0..expected.len() {
        assert_eq!(lexer.next_token(), expected[i])
    }
}
