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
            }
        }
    }

    Ok(())
}

fn eval(input: &str) {
    let lex = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lex);
    let program = parser.parse_program();
    if !parser.errors.is_empty() {
        parser.print_errors();
    }
    println!("{}\n", program);
}
