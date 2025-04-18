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
struct Token<'a> {
    kind: TokenType,
    literal: &'a str,
}

struct Lexer<'a> {
    input: &'a str,
    chars: Vec<char>,
    position: usize,
    read_position: usize,
}
impl<'a> Lexer<'a> {
    fn ch(&self) -> char {
        if self.read_position > self.input.len() {
            return '\0';
        }
        self.chars[self.position]
    }

    fn new(input: &'a str) -> Lexer {
        let mut lexer = Lexer {
            input,
            chars: input.chars().collect(),
            position: 0,
            read_position: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) -> &'a str {
        let start_byte = self
            .input
            .char_indices()
            .nth(self.position)
            .map_or(self.input.len(), |(i, _)| i);
        let end_byte = if self.position + 1 < self.chars.len() {
            self.input
                .char_indices()
                .nth(self.position + 1)
                .map_or(self.input.len(), |(i, _)| i)
        } else {
            self.input.len()
        };
        let char_slice = &self.input[start_byte..end_byte];
        self.position = self.read_position;
        self.read_position += 1;
        char_slice
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn read_molecule<F>(&mut self, predicate: F) -> &str
    where
        F: Fn(char) -> bool,
    {
        let position = self.position.clone();
        while predicate(self.ch()) {
            self.read_char();
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
        while Lexer::is_whitespace(self.ch()) {
            self.read_char();
        }
    }

    fn is_digit(ch: char) -> bool {
        ch.is_digit(10)
    }

    fn read_number(&mut self) -> &str {
        self.read_molecule(Lexer::is_digit)
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let atoms: HashMap<char, TokenType> = [
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
        .collect();
        let token = if let Some(atom) = atoms.get(&self.ch()) {
            Token {
                kind: atom.clone(),
                literal: self.read_char(),
            }
        } else if self.ch() == '\0' {
            Token {
                kind: TokenType::EOF,
                literal: "",
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
                literal: self.read_char(),
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
