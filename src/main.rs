mod lexer;
mod parser;

use std::fs;
use std::env;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    let filename = &args[1];

    let contents = fs::read(filename).expect("Something went wrong reading the file");
    let contents: Vec<char> = contents.into_iter().map(|c| c as char).collect();

    let tokens = Lexer::from_source_code(contents.into_iter().peekable());
    let mut parser = Parser::new(tokens.peekable());

    let expr = parser.parse_statement();

    println!("{:?}", expr)
}