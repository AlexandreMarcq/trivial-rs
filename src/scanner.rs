use std::{iter::Peekable, str::Chars};

use crate::token::{Lexeme, Literal, Operators, Token, TokenTypes, TwoChars};

const RESERVED_WORDS: [&str; 16] = [
    "and", "or", "if", "else", "for", "while", "false", "true", "class", "super", "this", "fun",
    "return", "print", "nil", "var",
];

pub struct Scanner<'source> {
    source: &'source str,
    iter: Peekable<Chars<'source>>,
    pub tokens: Vec<Option<Token<'source>>>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'source> Scanner<'source> {
    pub fn new(source: &'source str) -> Scanner<'source> {
        let iter = source.chars().peekable();
        Scanner {
            source,
            iter,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.current = self.current + 1;
        self.iter.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    fn match_next(&mut self, expected: char) -> bool {
        match self.peek() {
            Some(c) if *c == expected => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn add_token(&mut self, token_type: Option<TokenTypes>) {
        let text = &self.source[self.start..self.current];

        if let Some(t) = token_type {
            let lexeme = match t {
                TokenTypes::String => Lexeme::Literal(Literal::String(&text[1..text.len() - 1])),
                TokenTypes::Number => {
                    if text.contains('.') {
                        Lexeme::Literal(Literal::Float(
                            text.parse::<f64>()
                                .expect("could not convert number literal to u64"),
                        ))
                    } else {
                        Lexeme::Literal(Literal::Integer(
                            text.parse::<u64>()
                                .expect("could not convert number literal to f64"),
                        ))
                    }
                }
                TokenTypes::Identifier => Lexeme::Word(text),
                TokenTypes::ReservedWord(_) | TokenTypes::Operator(Operators::TwoChars(_)) => {
                    Lexeme::Word(text)
                }
                _ => Lexeme::Char(text.chars().last().expect("could not get char from text")),
            };

            self.tokens.push(Some(Token::new(t, lexeme, self.line)));
        } else {
            self.tokens.push(None);
            eprintln!("Unexpected character on line {}", self.line);
        }
    }

    fn scan_string(&mut self) {
        while let Some(c) = self.peek() {
            if *c != '"' {
                self.advance();
            } else {
                break;
            }
        }

        if let None = self.advance() {
            eprintln!("Unterminated string on line {}", self.line);
            return;
        }

        self.add_token(Some(TokenTypes::String));
    }

    fn scan_number(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_digit(10) {
                self.advance();
            } else {
                break;
            }
        }

        if let Some(c) = self.peek() {
            if *c == '.' {
                self.advance();

                while let Some(c) = self.peek() {
                    if c.is_digit(10) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
        }

        self.add_token(Some(TokenTypes::Number));
    }

    fn scan_identifier(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || *c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let word = &self.source[self.start..self.current];
        if RESERVED_WORDS.contains(&word) {
            self.add_token(Some(TokenTypes::ReservedWord(word.into())));
        } else {
            self.add_token(Some(TokenTypes::Identifier));
        }
    }

    pub fn scan_tokens(&mut self) {
        while let Some(char) = self.advance() {
            match char {
                '(' => self.add_token(Some(TokenTypes::LeftParen)),
                ')' => self.add_token(Some(TokenTypes::RightParen)),
                '[' => self.add_token(Some(TokenTypes::LeftBrace)),
                ']' => self.add_token(Some(TokenTypes::RightBrace)),
                ',' => self.add_token(Some(TokenTypes::Comma)),
                '.' => self.add_token(Some(TokenTypes::Dot)),
                ';' => self.add_token(Some(TokenTypes::Semicolon)),
                '-' => self.add_token(Some(TokenTypes::Operator(Operators::Minus))),
                '+' => self.add_token(Some(TokenTypes::Operator(Operators::Plus))),
                '/' => {
                    if self.match_next('/') {
                        while let Some(c) = self.peek() {
                            if *c != '\n' {
                                self.advance();
                            }
                        }
                    } else {
                        self.add_token(Some(TokenTypes::Operator(Operators::Slash)));
                    }
                }
                '*' => self.add_token(Some(TokenTypes::Operator(Operators::Star))),
                '!' => {
                    if self.match_next('=') {
                        self.add_token(Some(TokenTypes::Operator(Operators::TwoChars(
                            TwoChars::BangEqual,
                        ))));
                    } else {
                        self.add_token(Some(TokenTypes::Operator(Operators::Bang)));
                    }
                }
                '=' => {
                    if self.match_next('=') {
                        self.add_token(Some(TokenTypes::Operator(Operators::TwoChars(
                            TwoChars::EqualEqual,
                        ))));
                    } else {
                        self.add_token(Some(TokenTypes::Operator(Operators::Equal)));
                    }
                }
                '>' => {
                    if self.match_next('=') {
                        self.add_token(Some(TokenTypes::Operator(Operators::TwoChars(
                            TwoChars::GreaterEqual,
                        ))));
                    } else {
                        self.add_token(Some(TokenTypes::Operator(Operators::Greater)));
                    }
                }
                '<' => {
                    if self.match_next('=') {
                        self.add_token(Some(TokenTypes::Operator(Operators::TwoChars(
                            TwoChars::LessEqual,
                        ))));
                    } else {
                        self.add_token(Some(TokenTypes::Operator(Operators::Less)));
                    }
                }
                '"' => self.scan_string(),
                c if c.is_digit(10) => self.scan_number(),
                c if c.is_alphabetic() => self.scan_identifier(),
                '\n' => {
                    self.line = self.line + 1;
                }
                c if c.is_whitespace() => {}
                _ => self.add_token(None),
            }
            self.start = self.current;
        }

        self.tokens
            .push(Some(Token::new(TokenTypes::Eof, Lexeme::Eof, self.line)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_char() {
        let mut s = Scanner::new(";");
        s.scan_tokens();

        assert_eq!(
            vec![
                Some(Token::new(TokenTypes::Semicolon, Lexeme::Char(';'), 1)),
                Some(Token::new(TokenTypes::Eof, Lexeme::Eof, 1))
            ],
            s.tokens
        );
    }

    #[test]
    fn multiple_chars() {
        let mut s = Scanner::new("(),.");
        s.scan_tokens();

        assert_eq!(
            vec![
                Some(Token::new(TokenTypes::LeftParen, Lexeme::Char('('), 1,)),
                Some(Token::new(TokenTypes::RightParen, Lexeme::Char(')'), 1,)),
                Some(Token::new(TokenTypes::Comma, Lexeme::Char(','), 1,)),
                Some(Token::new(TokenTypes::Dot, Lexeme::Char('.'), 1,)),
                Some(Token::new(TokenTypes::Eof, Lexeme::Eof, 1,)),
            ],
            s.tokens
        );
    }

    #[test]
    fn double_chars() {
        let mut s = Scanner::new("!=");
        s.scan_tokens();

        assert_eq!(
            vec![
                Some(Token::new(
                    TokenTypes::Operator(Operators::TwoChars(TwoChars::BangEqual)),
                    Lexeme::Word("!="),
                    1
                )),
                Some(Token::new(TokenTypes::Eof, Lexeme::Eof, 1))
            ],
            s.tokens
        );
    }

    #[test]
    fn comment() {
        let mut s = Scanner::new("// this is a comment");
        s.scan_tokens();

        assert_eq!(
            vec![Some(Token::new(TokenTypes::Eof, Lexeme::Eof, 1))],
            s.tokens
        )
    }

    #[test]
    fn new_line() {
        let mut s = Scanner::new("+\n-");
        s.scan_tokens();

        assert_eq!(
            vec![
                Some(Token::new(
                    TokenTypes::Operator(Operators::Plus),
                    Lexeme::Char('+'),
                    1
                )),
                Some(Token::new(
                    TokenTypes::Operator(Operators::Minus),
                    Lexeme::Char('-'),
                    2
                )),
                Some(Token::new(TokenTypes::Eof, Lexeme::Eof, 2))
            ],
            s.tokens
        );
    }

    #[test]
    fn string() {
        let mut s = Scanner::new("\"test string\"");
        s.scan_tokens();

        assert_eq!(
            vec![
                Some(Token::new(
                    TokenTypes::String,
                    Lexeme::Literal(Literal::String("test string")),
                    1
                )),
                Some(Token::new(TokenTypes::Eof, Lexeme::Eof, 1))
            ],
            s.tokens,
        )
    }

    #[test]
    fn integer() {
        let mut s = Scanner::new("12");
        s.scan_tokens();

        assert_eq!(
            vec![
                Some(Token::new(
                    TokenTypes::Number,
                    Lexeme::Literal(Literal::Integer(12)),
                    1
                )),
                Some(Token::new(TokenTypes::Eof, Lexeme::Eof, 1))
            ],
            s.tokens,
        );
    }

    #[test]
    fn float() {
        let mut s = Scanner::new("12.34");
        s.scan_tokens();

        assert_eq!(
            vec![
                Some(Token::new(
                    TokenTypes::Number,
                    Lexeme::Literal(Literal::Float(12.34)),
                    1
                )),
                Some(Token::new(TokenTypes::Eof, Lexeme::Eof, 1))
            ],
            s.tokens,
        );
    }

    #[test]
    fn identifier() {
        let mut s = Scanner::new("test");
        s.scan_tokens();

        assert_eq!(
            vec![
                Some(Token::new(TokenTypes::Identifier, Lexeme::Word("test"), 1)),
                Some(Token::new(TokenTypes::Eof, Lexeme::Eof, 1))
            ],
            s.tokens,
        );
    }

    #[test]
    fn reserved_word() {
        use crate::token::ReservedWords;

        let mut s = Scanner::new("while");
        s.scan_tokens();

        assert_eq!(
            vec![
                Some(Token::new(
                    TokenTypes::ReservedWord(ReservedWords::While),
                    Lexeme::Word("while"),
                    1
                )),
                Some(Token::new(TokenTypes::Eof, Lexeme::Eof, 1)),
            ],
            s.tokens
        );
    }
}
