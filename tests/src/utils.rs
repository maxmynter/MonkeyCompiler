use ast::Program;
use lexer::Lexer;
use parser::Parser;

pub fn prepare_program_for_test(input: &str) -> Program {
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(p);
    program
}

fn check_parse_errors(p: Parser) {
    println!("Parser has {} errors", p.errors.len());
    for msg in &p.errors {
        eprintln!("Error: {}", msg);
    }
    assert_eq!(p.errors.len(), 0);
}
