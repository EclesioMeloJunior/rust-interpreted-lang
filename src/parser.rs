use std::iter::Peekable;
use crate::lexer::{Token, TokenKind};

#[derive(Debug)]
pub enum UnaryOperator {
    Neg,
    Min,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Sub,
    Sum,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum Expr {
    Int(i32),
    Iden(String),
    Grouping(Vec<Expr>),
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

const LOWEST_PRECEDENCE: u8 = 0;
const TERM_PRECEDENCE:  u8 = 1;

fn is_binary_operator(token: &Token) -> bool {
    match token.kind {
        TokenKind::Plus => true,
        _ => false
    }
}

pub struct Parser<T: Iterator<Item=Token>> {
    tokens: Peekable<T>,
    program: Vec<Expr>
}

impl<T: Iterator<Item=Token>> Parser<T> {
    fn parse_expression(&mut self, precedence: u8) -> Expr {
        let current_token: Token;
        if let Some(token) = self.tokens.next() {
            current_token = token;
        } else {
            todo!("implement parse error: Empty token while parsing expression")
        }

        match current_token.kind {
            TokenKind::Ident => {
                return Expr::Iden(current_token.lexeme);
            },
            TokenKind::Number => {
                let i32_number = current_token.lexeme.parse::<i32>().unwrap();
                return Expr::Int(i32_number);
            },
            TokenKind::OpenParen => {
                let expression = self.parse_expression(precedence);
                let mut grouped_expressions: Vec<Expr> = vec![expression];

                if let Some(next_token) = self.tokens.peek() {
                    match next_token.kind {
                        TokenKind::CloseParen => {
                            return Expr::Grouping(grouped_expressions);
                        }
                        TokenKind::SemiColon => todo!("implement parse error: Expected close paren"),
                        _ => grouped_expressions.push(self.parse_expression(precedence)),
                    }
                }

                todo!("implement parse error: Expected close paren")
            },
            _ => todo!("implement parse error: unexpected token while parsing expression")
        };
    }

    pub fn parse_statement(&mut self) -> Expr {
        let mut lhs = self.parse_expression(LOWEST_PRECEDENCE);

        while let Some(token) = self.tokens.next_if(|token| is_binary_operator(token)) {
            match token.kind {
                TokenKind::Plus => {
                    let rhs: Expr = self.parse_expression(TERM_PRECEDENCE);
                    let binary_exp: Expr = Expr::BinaryExpr{
                        op: BinaryOperator::Sum,
                        rhs: Box::new(rhs),
                        lhs: Box::new(lhs)
                    };

                    lhs = binary_exp
                }
                TokenKind::SemiColon => {
                    return lhs;
                }
                _ => todo!("implement parse error: token not supported")
            }
        }

        return lhs;
    }

    pub fn new(tokens_iter: Peekable<T>) -> Self {
        return Parser{
            tokens: tokens_iter,
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