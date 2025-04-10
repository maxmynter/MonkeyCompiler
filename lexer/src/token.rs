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
        self.input[self.read_position]
    }
}

impl Lexer {
    fn new(input: String) -> Lexer {
        Lexer {
            input: input.into_bytes(),
            position: 0,
            read_position: 0,
        }
    }
    fn read_char(&mut self) {
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        let token = match self.ch() {
            b'=' => Token {
                literal: self.ch().to_string(),
                kind: TokenType::ASSIGN,
            },
            _ => {
                todo!("Add additional");
                unreachable!("Why am i here")
            }
        };
        self.read_char();
        token
    }
}

#[test]
fn test_eat_token() {
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
