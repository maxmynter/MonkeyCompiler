use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use compiler::Compiler;
use object::{Environment, ObjectTraits};

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

fn eval(input: &str, _env: Rc<RefCell<Environment>>) {
    let lex = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lex);
    let program = parser.parse_program();
    if !parser.errors.is_empty() {
        parser.print_errors();
    }
    let mut comp = Compiler::new();
    if let Err(err) = comp.compile(program) {
        println!("Error: {}", err);
    }
    let mut machine = vm::VM::new(comp.bytecode());
    let result = machine.run();
    if let Err(e) = result {
        println!("{:?}", e);
    } else {
        let stack_top = machine.last_popped_stack_elem().unwrap();
        println!("{}\n", stack_top.inspect());
    }
}
