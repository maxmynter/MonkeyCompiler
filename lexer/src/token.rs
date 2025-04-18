use std::collections::HashMap;

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

    fn read_char(&mut self) -> String {
        let char = &self.input[self.position..self.read_position];
        self.position = self.read_position;
        self.read_position += 1;
        std::str::from_utf8(&char)
            .expect("Could not read char")
            .to_string()
    }

    fn is_letter(ch: u8) -> bool {
        b'a' <= ch && ch <= b'z' || b'A' <= ch && ch <= b'Z' || ch == b'_'
    }

    fn read_molecule<F>(&mut self, predicate: F) -> String
    where
        F: Fn(u8) -> bool,
    {
        let position = self.position.clone();
        while predicate(self.ch()) {
            self.read_char();
        }
        std::str::from_utf8(&self.input[position..self.position])
            .expect("Could not parse")
            .to_string()
    }
    fn read_identifier(&mut self) -> String {
        self.read_molecule(Lexer::is_letter)
    }

    fn is_whitespace(ch: u8) -> bool {
        ch == b' ' || ch == b'\t' || ch == b'\n' || ch == b'\r'
    }

    fn skip_whitespace(&mut self) {
        while Lexer::is_whitespace(self.ch()) {
            self.read_char();
        }
    }

    fn is_digit(ch: u8) -> bool {
        b'0' <= ch && ch <= b'9'
    }

    fn read_number(&mut self) -> String {
        self.read_molecule(Lexer::is_digit)
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let atoms: HashMap<u8, TokenType> = [
            (b'=', TokenType::ASSIGN),
            (b'+', TokenType::PLUS),
            (b'(', TokenType::LPAREN),
            (b')', TokenType::RPAREN),
            (b'{', TokenType::LBRACE),
            (b'}', TokenType::RBRACE),
            (b';', TokenType::SEMICOLON),
            (b',', TokenType::COMMA),
        ]
        .iter()
        .cloned()
        .collect();
        let token = if let Some(atom) = atoms.get(&self.ch()) {
            let char_str = self.read_char();
            Token {
                kind: atom.clone(),
                literal: char_str,
            }
        } else if self.ch() == 0 {
            Token {
                kind: TokenType::EOF,
                literal: String::new(),
            }
        } else if Lexer::is_letter(self.ch()) {
            let identifier = self.read_identifier();
            Token {
                kind: Keywords::lookup_ident(&identifier),
                literal: identifier,
            }
        } else if Lexer::is_digit(self.ch()) {
            let number = self.read_number();
            Token {
                kind: TokenType::INT,
                literal: number,
            }
        } else {
            Token {
                kind: TokenType::ILLEGAL,
                literal: self.ch_str(),
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
        let tok = lexer.next_token();
        println!("{:?}-:-{:?}", &tok, &expected[i]);
        assert_eq!(tok, expected[i])
    }
}
