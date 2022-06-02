use std::iter::Peekable;
use crate::lexer::{Token};


pub enum UnaryOperator {
    Neg,
    Min,
}

pub enum BinaryOperator {
    Sub,
    Sum,
    Mul,
    Div,
}

pub enum Expr {
    Int(i32),
    UnaryExpr {
        op: UnaryOperator,
        rhs: Box<Expr>
    },
    BinaryExpr {
        op: BinaryOperator,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    }
}

pub struct Statement (Vec<Expr>);

pub struct Parser {
    program: Statement
}

impl Parser {
    pub fn new<T: Iterator<Item=Token>>(tokens_iter: Peekable<T>) -> Self {
        return Parser{
            program: vec![],
        }
    }
}

#[cfg(test)]
mod test {
    
    #[test]
    fn parse_simple_expression() {
    }
}