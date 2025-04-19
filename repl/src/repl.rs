use std::io::{self, stdin, Write};

const PROMPT: &str = ">>";

fn start() -> io::Result<()> {
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
                let result = eval(input);
                println!("{}", result);
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                break;
            }
        }
    }

    Ok(())
}

fn eval(input: &str) -> String {
    todo!();
}
