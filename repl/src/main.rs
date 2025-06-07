use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use object::{CoerceObject, Environment, ObjectTraits};

const PROMPT: &str = ">> ";

fn main() -> io::Result<()> {
    println!("Welcome to the Monkey programming language!");
    println!("Type commands.");
    repl()
}

fn repl() -> io::Result<()> {
    let stdin = io::stdin();
    let mut input = String::new();

    let mut stdout = io::stdout();

    let env = Environment::new();
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
                eval(input, env.clone());
            }
            Err(e) => {
                eprintln!("Error: {}", e);
            }
        }
    }

    Ok(())
}

fn eval(input: &str, env: Rc<RefCell<Environment>>) {
    let lex = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lex);
    let program = parser.parse_program();
    if !parser.errors.is_empty() {
        parser.print_errors();
    }
    let evaluated = match program.evaluate(&env) {
        Ok(result) => result.inspect(),
        Err(result) => result.inspect(),
    };
    println!("{}\n", evaluated);
}
