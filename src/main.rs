use std::fmt;
use std::iter::Peekable;
use std::iter::Iterator;

#[derive(Debug, Clone)]
struct LexerError;

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "lexer error (tbd)")
    }
}

#[derive(Debug, PartialEq)]
enum TokenKind {
    Fn,
    Ident,
    OpenParen, CloseParen,
    OpenBrac, CloseBrac,
    SemiColon,
    Number,
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    lexeme: String,
}

#[derive(Debug, Clone)]
struct Lexer<T: Iterator<Item=char>> {
    source: Peekable<T>,
}

impl<T: Iterator<Item=char>> Lexer<T> {
    fn from_source_code(source: Peekable<T>) -> Self {
        return Lexer {
            source: source
        }
    }
}

impl<T: Iterator<Item=char>> Iterator for Lexer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        while self.source.next_if(|x| x.is_whitespace()).is_some() {}

        let mut lexeme: Vec<char> = vec![];
       
        while let Some(char_) = self.source.next_if(|x| !x.is_whitespace()) {
            match char_ {
                '{' =>  {
                    return Some(Token {
                        lexeme: "{".to_string(),
                        kind: TokenKind::OpenBrac,
                    })
                },
                '}' =>  {
                    return Some(Token {
                        lexeme: "}".to_string(),
                        kind: TokenKind::CloseBrac,
                    })
                },
                '(' =>  {
                    return Some(Token {
                        lexeme: "(".to_string(),
                        kind: TokenKind::OpenParen,
                    })
                },
                ')' =>  {
                    return Some(Token {
                        lexeme: ")".to_string(),
                        kind: TokenKind::CloseParen,
                    })
                },
                ';' =>  {
                    return Some(Token {
                        lexeme: ";".to_string(),
                        kind: TokenKind::SemiColon,
                    })
                },
                _ => {
                    lexeme.push(char_);

                    if char_.is_numeric() {
                        while let Some(numeric) = self.source.next_if(|x| x.is_numeric()) {
                            lexeme.push(numeric);
                        }

                        return Some(Token {
                            kind: TokenKind::Number,
                            lexeme: String::from_iter(lexeme),
                        })
                    }
                    
                    if char_.is_ascii_alphabetic() {
                        while let Some(alphabetic) = self.source.next_if(|x| x.is_ascii_alphabetic()) {
                            lexeme.push(alphabetic);
                        }

                        let lexeme = String::from_iter(lexeme);

                        return match lexeme.as_ref() {
                            "fn" => Some(Token {
                                kind: TokenKind::Fn,
                                lexeme: lexeme,
                            }),
                            _ => Some(Token {
                                kind: TokenKind::Ident,
                                lexeme: lexeme,
                            })
                        }
                    }
                }
            }
        }

        return None
    }
}

fn main() {
    println!("hello world");
}

#[cfg(test)]
mod tests {
    use super::*;

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
            }
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