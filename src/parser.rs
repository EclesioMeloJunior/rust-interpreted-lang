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
    Sentinel,
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
        TokenKind::Plus | TokenKind::Minus | TokenKind::Slash | TokenKind::Star => true,
        _ => false,
    }
}

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,

    operators: Vec<Expr>,
    operands: Vec<Expr>,
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
                let i32_number = current_token.lexeme.parse::<i32>().unwrap();
                self.operands.push(Expr::Int(i32_number));
            }
            TokenKind::OpenParen => {
                let expression = self.parse_expression_statement().unwrap();
                let close_paren = self.tokens.next();

                match close_paren {
                    Some(token) => {
                        if token.kind == TokenKind::CloseParen {
                            self.operators.push(Expr::Sentinel);
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
        Ok()
    }

    pub fn parse_expression_statement(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_expression().unwrap();

        while let Some(token) = self.tokens.next_if(|token| is_binary_operator(token)) {
            let rhs = self.parse_expression();
            if rhs.is_err() {
                return rhs;
            }

            lhs = match token.kind {
                TokenKind::Slash => Expr::BinaryExpr {
                    op: BinaryOperator::Div,
                    rhs: Box::new(rhs.unwrap()),
                    lhs: Box::new(lhs),
                },
                TokenKind::Star => Expr::BinaryExpr {
                    op: BinaryOperator::Mul,
                    rhs: Box::new(rhs.unwrap()),
                    lhs: Box::new(lhs),
                },
                TokenKind::Minus => Expr::BinaryExpr {
                    op: BinaryOperator::Sub,
                    rhs: Box::new(rhs.unwrap()),
                    lhs: Box::new(lhs),
                },
                TokenKind::Plus => Expr::BinaryExpr {
                    op: BinaryOperator::Sum,
                    rhs: Box::new(rhs.unwrap()),
                    lhs: Box::new(lhs),
                },
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
