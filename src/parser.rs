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

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Sub,
    Sum,
    Mul,
    Div,
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

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Int(i32),
    Iden(String),
    Grouping(Box<Expr>),
    UnaryExpr {
        op: UnaryOperator,
        rhs: Box<Expr>,
    },
    BinaryExpr {
        op: BinaryOperator,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Int(i) => write!(f, "{}", i),
            Expr::Iden(value) => write!(f, "{}", value),
            Expr::Grouping(expr) => write!(f, "({})", expr),
            Expr::BinaryExpr { op, lhs, rhs } => return write!(f, "{} {} {}", lhs, op, rhs),
            _ => Err(std::fmt::Error),
        }
    }
}

const LOWEST_PRECEDENCE: u8 = 0;
const TERM_PRECEDENCE: u8 = 1;

fn is_binary_operator(token: &Token) -> bool {
    match token.kind {
        TokenKind::Plus => true,
        _ => false,
    }
}

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        let current_token: Token;
        if let Some(token) = self.tokens.next() {
            current_token = token;
        } else {
            todo!("implement parse error: Empty token while parsing expression")
        }

        match current_token.kind {
            TokenKind::Ident => Ok(Expr::Iden(current_token.lexeme)),
            TokenKind::Number => {
                let i32_number = current_token.lexeme.parse::<i32>().unwrap();
                Ok(Expr::Int(i32_number))
            }
            TokenKind::OpenParen => {
                let expression = self.parse_expression_statement().unwrap();
                let close_paren = self.tokens.next();

                match close_paren {
                    Some(token) => {
                        if token.kind == TokenKind::CloseParen {
                            return Ok(Expr::Grouping(Box::new(expression)));
                        }

                        Err(ParserError(format!(
                            "expected close parentheses, got {}",
                            token.lexeme
                        )))
                    }
                    None => Err(ParserError(String::from(
                        "expected close parentheses, got none",
                    ))),
                }
            }
            _ => Err(ParserError(format!(
                "unexpected token while parsing: {}",
                current_token.lexeme
            ))),
        }
    }

    pub fn parse_expression_statement(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_expression().unwrap();

        while let Some(token) = self.tokens.next_if(|token| is_binary_operator(token)) {
            match token.kind {
                TokenKind::Plus => {
                    let rhs = self.parse_expression();
                    if rhs.is_err() {
                        return rhs;
                    }

                    let binary_exp: Expr = Expr::BinaryExpr {
                        op: BinaryOperator::Sum,
                        rhs: Box::new(rhs.unwrap()),
                        lhs: Box::new(lhs),
                    };

                    lhs = binary_exp
                }
                _ => {
                    return Err(ParserError(format!(
                        "expected any of +, -, *, / got: {}",
                        token.lexeme
                    )))
                }
            }
        }

        Ok(lhs)
    }

    pub fn parse_statement(&mut self) -> Result<Expr, ParserError> {
        self.parse_expression_statement()
    }

    pub fn new(tokens_iter: Peekable<T>) -> Self {
        return Parser {
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
            lhs: Box::new(Expr::BinaryExpr {
                op: BinaryOperator::Sum,
                lhs: Box::new(Expr::Int(10)),
                rhs: Box::new(Expr::Int(20)),
            }),
            rhs: Box::new(Expr::Int(30)),
        };

        let parsed_expression = parser.parse_statement();

        assert_eq!(parsed_expression, expected_expression);
    }
}
