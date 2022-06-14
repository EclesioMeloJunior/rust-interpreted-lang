#![allow(dead_code)]

use crate::lexer::{Token, TokenKind};
use std::iter::Peekable;

pub struct ParserError(String);
impl std::fmt::Debug for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[DEBUG] error while parsing: {}", self.0)
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error while parsing: {}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Neg,
    Min,
}

impl UnaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            UnaryOperator::Neg | UnaryOperator::Min => 3,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Sub,
    Sum,
    Mul,
    Div,
}

impl BinaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Sum | BinaryOperator::Sub => 1,
            BinaryOperator::Mul => 2,
            BinaryOperator::Div => 3,
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Sum => write!(f, "+"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum Expr {
    Sentinel,
    Int32(String),
    Iden(String),
    Grouping(Box<Expr>),
    UnaryExpr {
        op: UnaryOperator,
        rhs: Box<Expr>,
    },
    BinaryExpr {
        op: BinaryOperator,
        lhs: Option<Box<Expr>>,
        rhs: Option<Box<Expr>>,
    },
}

impl Expr {
    fn precedence(&self) -> u8 {
        match self {
            Expr::BinaryExpr { op, .. } => op.precedence(),
            _ => 0,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Int32(i) => write!(f, "{}", i),
            Expr::Iden(value) => write!(f, "{}", value),
            Expr::Grouping(expr) => write!(f, "({})", expr),
            Expr::BinaryExpr { op, lhs, rhs } => {
                return write!(
                    f,
                    "({} {} {})",
                    lhs.as_ref().unwrap(),
                    op,
                    rhs.as_ref().unwrap()
                )
            }
            _ => Err(std::fmt::Error),
        }
    }
}

fn is_binary_operator(token: &Token) -> bool {
    match token.kind {
        TokenKind::Plus | TokenKind::Minus | TokenKind::Slash | TokenKind::Star => true,
        _ => false,
    }
}

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,

    operators: Vec<Expr>,
    pub operands: Vec<Expr>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    fn parse_expression(&mut self) -> Result<(), ParserError> {
        let current_token: Token;
        if let Some(token) = self.tokens.next() {
            current_token = token;
        } else {
            todo!("implement parse error: Empty token while parsing expression")
        }

        match current_token.kind {
            TokenKind::Ident => {
                self.operands.push(Expr::Iden(current_token.lexeme));
            }
            TokenKind::Number => {
                self.operands.push(Expr::Int32(current_token.lexeme));
            }
            TokenKind::OpenParen => {
                self.operators.push(Expr::Sentinel);
                self.parse_expression_statement().unwrap();
                let close_paren = self.tokens.next();

                match close_paren {
                    Some(token) => {
                        if token.kind == TokenKind::CloseParen {
                            self.operators.pop();
                        } else {
                            return Err(ParserError(format!(
                                "expected close parentheses, got {}",
                                token.lexeme
                            )));
                        }
                    }
                    None => {
                        return Err(ParserError(String::from(
                            "expected close parentheses, got none",
                        )))
                    }
                }
            }
            _ => {
                return Err(ParserError(format!(
                    "unexpected token while parsing: {}",
                    current_token.lexeme
                )))
            }
        };
        Ok(())
    }

    pub fn pop_operators(&mut self) {
        if let Some(on_top_expression) = self.operators.pop() {
            match on_top_expression {
                Expr::BinaryExpr { op, .. } => {
                    let rhs = self.operands.pop().unwrap();
                    let lhs = self.operands.pop().unwrap();

                    self.operands.push(Expr::BinaryExpr {
                        op: op,
                        lhs: Some(Box::new(lhs)),
                        rhs: Some(Box::new(rhs)),
                    })
                }
                _ => {}
            }
        }
    }

    pub fn push_operator(&mut self, expression_to_add: Expr) {
        let mut operator_on_top = self.operators.last();
        while operator_on_top.is_some() {
            let current_operator = operator_on_top.unwrap();

            if current_operator.precedence() > expression_to_add.precedence() {
                self.pop_operators();
                operator_on_top = self.operators.last();
                continue;
            }

            break;
        }

        self.operators.push(expression_to_add)
    }

    pub fn parse_expression_statement(&mut self) -> Result<(), ParserError> {
        self.parse_expression().unwrap();

        while let Some(token) = self.tokens.next_if(|token| is_binary_operator(token)) {
            match token.kind {
                TokenKind::Slash => self.push_operator(Expr::BinaryExpr {
                    op: BinaryOperator::Div,
                    rhs: None,
                    lhs: None,
                }),
                TokenKind::Star => self.push_operator(Expr::BinaryExpr {
                    op: BinaryOperator::Mul,
                    rhs: None,
                    lhs: None,
                }),
                TokenKind::Minus => self.push_operator(Expr::BinaryExpr {
                    op: BinaryOperator::Sub,
                    rhs: None,
                    lhs: None,
                }),
                TokenKind::Plus => self.push_operator(Expr::BinaryExpr {
                    op: BinaryOperator::Sum,
                    rhs: None,
                    lhs: None,
                }),
                _ => {
                    return Err(ParserError(format!(
                        "expected any of +, -, *, / got: {}",
                        token.lexeme
                    )));
                }
            }

            self.parse_expression().unwrap()
        }

        loop {
            let on_top_operator = self.operators.last();
            match on_top_operator {
                Some(operator) => match operator {
                    Expr::Sentinel => break,
                    _ => self.pop_operators(),
                },
                None => break,
            }
        }
        Ok(())
    }

    pub fn parse_statement(&mut self) -> Result<(), ParserError> {
        self.parse_expression_statement()
    }

    pub fn new(tokens_iter: Peekable<T>) -> Self {
        return Parser {
            operands: vec![],
            operators: vec![Expr::Sentinel],
            tokens: tokens_iter,
        };
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn parse_simple_expression() {
        let source: String = "10 + 20 + 30".to_string();
        let lexer = Lexer::from_source_code(source.chars().peekable());

        let mut parser = Parser::new(lexer.peekable());

        let expected_expression = Expr::BinaryExpr {
            op: BinaryOperator::Sum,
            lhs: Some(Box::new(Expr::BinaryExpr {
                op: BinaryOperator::Sum,
                lhs: Some(Box::new(Expr::Int32(String::from("10")))),
                rhs: Some(Box::new(Expr::Int32(String::from("20")))),
            })),
            rhs: Some(Box::new(Expr::Int32(String::from("30")))),
        };

        let parsed_expression = parser.parse_statement();

        assert_eq!(parsed_expression, expected_expression);
    }
}
