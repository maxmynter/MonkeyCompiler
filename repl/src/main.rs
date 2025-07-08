use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use compiler::{symbol_table::SymbolTable, Compiler};
use object::{Environment, Object, ObjectTraits};
use vm::GLOBALS_SIZE;

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
    let mut constants: Vec<Object> = Vec::new();
    let mut globals = vec![Object::Null; GLOBALS_SIZE];
    let mut symbol_table = SymbolTable::new();
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
                (constants, symbol_table, globals) =
                    eval(input, env.clone(), constants, symbol_table, globals);
            }
            Err(e) => {
                eprintln!("Error: {}", e);
            }
        }
    }

    Ok(())
}

fn eval(
    input: &str,
    env: Rc<RefCell<Environment>>,
    constants: Vec<Object>,
    symbols: SymbolTable,
    globals: Vec<Object>,
) -> (Vec<Object>, SymbolTable, Vec<Object>) {
    let lex = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lex);
    let program = parser.parse_program();
    if !parser.errors.is_empty() {
        parser.print_errors();
    }
    let mut comp = Compiler::new_with_state(constants, symbols);
    if let Err(err) = comp.compile(program) {
        println!("Error: {}", err);
    }
    let mut machine = vm::VM::new_with_global_store(comp.bytecode(), globals);
    let result = machine.run();
    if let Err(e) = result {
        println!("{:?}", e);
    } else {
        let stack_top = machine.last_popped_stack_elem().unwrap();
        println!("{}\n", stack_top.inspect());
    }
    (comp.constants, comp.symbol_table, machine.get_globals())
}
