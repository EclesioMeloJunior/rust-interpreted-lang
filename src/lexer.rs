use std::iter::Iterator;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Plus,
    Minus,
    Slash,
    Star,
    Let,
    Fn,
    Ident,
    OpenParen,
    CloseParen,
    OpenBrac,
    CloseBrac,
    SemiColon,
    Number,
    Equals,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
}

#[derive(Debug, Clone)]
pub struct Lexer<T: Iterator<Item = char>> {
    source: Peekable<T>,
}

impl<T: Iterator<Item = char>> Lexer<T> {
    pub fn from_source_code(source: Peekable<T>) -> Self {
        return Lexer { source: source };
    }
}

impl<T: Iterator<Item = char>> Iterator for Lexer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        while self.source.next_if(|x| x.is_whitespace()).is_some() {}

        let mut lexeme: Vec<char> = vec![];
        while let Some(char_) = self.source.next_if(|x| !x.is_whitespace()) {
            return match char_ {
                '{' => Some(Token {
                    lexeme: "{".to_string(),
                    kind: TokenKind::OpenBrac,
                }),
                '}' => Some(Token {
                    lexeme: "}".to_string(),
                    kind: TokenKind::CloseBrac,
                }),
                '(' => Some(Token {
                    lexeme: "(".to_string(),
                    kind: TokenKind::OpenParen,
                }),
                ')' => Some(Token {
                    lexeme: ")".to_string(),
                    kind: TokenKind::CloseParen,
                }),
                ';' => Some(Token {
                    lexeme: ";".to_string(),
                    kind: TokenKind::SemiColon,
                }),
                '=' => Some(Token {
                    lexeme: "=".to_string(),
                    kind: TokenKind::Equals,
                }),
                '+' => Some(Token {
                    lexeme: "+".to_string(),
                    kind: TokenKind::Plus,
                }),
                '-' => Some(Token {
                    lexeme: "-".to_string(),
                    kind: TokenKind::Minus,
                }),
                '*' => Some(Token {
                    lexeme: "*".to_string(),
                    kind: TokenKind::Star,
                }),
                '/' => Some(Token {
                    lexeme: "/".to_string(),
                    kind: TokenKind::Slash,
                }),
                _ => {
                    lexeme.push(char_);

                    if char_.is_numeric() {
                        while let Some(numeric) = self.source.next_if(|x| x.is_numeric()) {
                            lexeme.push(numeric);
                        }

                        return Some(Token {
                            kind: TokenKind::Number,
                            lexeme: String::from_iter(lexeme),
                        });
                    }
                    if char_.is_ascii_alphabetic() {
                        while let Some(alphabetic) =
                            self.source.next_if(|x| x.is_ascii_alphabetic())
                        {
                            lexeme.push(alphabetic);
                        }

                        let lexeme = String::from_iter(lexeme);

                        return match lexeme.as_ref() {
                            "let" => Some(Token {
                                kind: TokenKind::Let,
                                lexeme: lexeme,
                            }),
                            "fn" => Some(Token {
                                kind: TokenKind::Fn,
                                lexeme: lexeme,
                            }),
                            _ => Some(Token {
                                kind: TokenKind::Ident,
                                lexeme: lexeme,
                            }),
                        };
                    }

                    return None;
                }
            };
        }

        return None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexing_tokens() {
        let source: String = String::from("let fn() {} a; y=b");
        let expected_tokens: Vec<Token> = vec![
            Token {
                kind: TokenKind::Let,
                lexeme: String::from("let"),
            },
            Token {
                kind: TokenKind::Fn,
                lexeme: String::from("fn"),
            },
            Token {
                kind: TokenKind::OpenParen,
                lexeme: String::from("("),
            },
            Token {
                kind: TokenKind::CloseParen,
                lexeme: String::from(")"),
            },
            Token {
                kind: TokenKind::OpenBrac,
                lexeme: String::from("{"),
            },
            Token {
                kind: TokenKind::CloseBrac,
                lexeme: String::from("}"),
            },
            Token {
                kind: TokenKind::Ident,
                lexeme: String::from("a"),
            },
            Token {
                kind: TokenKind::SemiColon,
                lexeme: String::from(";"),
            },
            Token {
                kind: TokenKind::Ident,
                lexeme: String::from("y"),
            },
            Token {
                kind: TokenKind::Equals,
                lexeme: String::from("="),
            },
            Token {
                kind: TokenKind::Ident,
                lexeme: String::from("b"),
            },
        ];

        let mut output_iter = Lexer::from_source_code(source.chars().peekable());
        let tokens: Vec<Token> = output_iter.by_ref().collect::<Vec<Token>>();

        assert_eq!(tokens.len(), expected_tokens.len());

        for (got, exp) in tokens.iter().zip(expected_tokens.iter()) {
            assert_eq!(got.kind, exp.kind);
            assert_eq!(got.lexeme, exp.lexeme);
        }
    }

    #[test]
    fn test_read_simple_main_function() {
        let source: String = String::from("fn main() {}");
        let expected_tokens: Vec<Token> = vec![
            Token {
                kind: TokenKind::Fn,
                lexeme: String::from("fn"),
            },
            Token {
                kind: TokenKind::Ident,
                lexeme: String::from("main"),
            },
            Token {
                kind: TokenKind::OpenParen,
                lexeme: String::from("("),
            },
            Token {
                kind: TokenKind::CloseParen,
                lexeme: String::from(")"),
            },
            Token {
                kind: TokenKind::OpenBrac,
                lexeme: String::from("{"),
            },
            Token {
                kind: TokenKind::CloseBrac,
                lexeme: String::from("}"),
            },
        ];

        let mut tokens_iter = Lexer::from_source_code(source.chars().peekable());
        let tokens_vec = tokens_iter.by_ref().collect::<Vec<Token>>();
        assert_eq!(tokens_vec.len(), expected_tokens.len());

        for (got, exp) in tokens_vec.iter().zip(expected_tokens.iter()) {
            assert_eq!(got.kind, exp.kind);
            assert_eq!(got.lexeme, exp.lexeme);
        }
    }
}
