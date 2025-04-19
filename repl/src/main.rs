use lexer;
use std::io::{self, Write};

const PROMPT: &str = ">>";

fn main() -> io::Result<()> {
    println!("Welcome to the Monkey programming language!");
    println!("Type commands.");
    repl()
}

fn repl() -> io::Result<()> {
    let stdin = io::stdin();
    let mut input = String::new();

    let mut stdout = io::stdout();
    loop {
        print!("{}", PROMPT);
        stdout.flush()?;
        input.clear();
        match stdin.read_line(&mut input) {
            Ok(0) => {
                println!("\nexiting...");
                break;
            }
            Ok(_) => {
                let input = input.trim();
                if input == "exit" || input == "quit" {
                    println!("\nexiting...");
                    break;
                }
                eval(input);
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                break;
            }
        }
    }

    Ok(())
}

fn eval(input: &str) {
    let mut lex = lexer::Lexer::new(input);
    loop {
        let tok = lex.next_token();
        println!("{}", tok.literal);
        if tok.kind == lexer::TokenType::EOF {
            break;
        }
    }
}
