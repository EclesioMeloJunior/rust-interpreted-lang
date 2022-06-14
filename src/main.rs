mod lexer;
mod parser;

use lexer::Lexer;
use parser::{BinaryOperator, Expr, Parser};
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let contents = fs::read(filename).expect("Something went wrong reading the file");
    let contents: Vec<char> = contents.into_iter().map(|c| c as char).collect();

    let tokens = Lexer::from_source_code(contents.into_iter().peekable());
    let mut parser = Parser::new(tokens.peekable());

    parser
        .parse_statement()
        .expect("failed to parse the ril file");

    let root_expression = &parser.operands[0];
    let mut instructions: Vec<Instruction> = vec![];
    to_bytecode(root_expression, &mut instructions);

    println!("{:?}", instructions);
}

#[derive(Debug)]
enum OpCode {
    LOAD_I32,
    ADD_I32,
    SUB_I32,
    MUL_I32,
    DIV_I32,
}

#[derive(Debug)]
struct Instruction {
    op_code: OpCode,
    args: Option<Vec<String>>,
}

fn to_bytecode(expr: &Expr, instructions: &mut Vec<Instruction>) -> () {
    match expr {
        Expr::Int32(value) => instructions.push(Instruction {
            op_code: OpCode::LOAD_I32,
            args: Some(vec![value.clone()]),
        }),
        Expr::BinaryExpr { op, lhs, rhs } => {
            match lhs {
                Some(lhs_expression) => to_bytecode(lhs_expression, instructions),
                _ => (),
            }

            match rhs {
                Some(rhs_expression) => to_bytecode(rhs_expression, instructions),
                _ => (),
            }

            match op {
                BinaryOperator::Sub => instructions.push(Instruction {
                    op_code: OpCode::SUB_I32,
                    args: None,
                }),
                BinaryOperator::Sum => instructions.push(Instruction {
                    op_code: OpCode::ADD_I32,
                    args: None,
                }),
                BinaryOperator::Mul => instructions.push(Instruction {
                    op_code: OpCode::MUL_I32,
                    args: None,
                }),
                BinaryOperator::Div => instructions.push(Instruction {
                    op_code: OpCode::DIV_I32,
                    args: None,
                }),
            }
        }
        _ => (),
    };
}
