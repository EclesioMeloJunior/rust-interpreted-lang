mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    let filename = &args[1];

    let contents = fs::read(filename).expect("Something went wrong reading the file");
    let contents: Vec<char> = contents.into_iter().map(|c| c as char).collect();

    let tokens = Lexer::from_source_code(contents.into_iter().peekable());
    let mut parser = Parser::new(tokens.peekable());

    match parser.parse_statement() {
        Ok(_) => println!("{}", parser.operands[0]),
        Err(err) => println!("{}", err),
    }
}
