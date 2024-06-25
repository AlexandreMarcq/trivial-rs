use core::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReservedWords {
    And,
    Or,
    If,
    Else,
    For,
    While,
    False,
    True,
    Class,
    Super,
    This,
    Fun,
    Return,
    Print,
    Nil,
    Var,
}

impl From<&str> for ReservedWords {
    fn from(value: &str) -> Self {
        match value {
            "and" => ReservedWords::And,
            "or" => ReservedWords::Or,
            "if" => ReservedWords::If,
            "else" => ReservedWords::Else,
            "for" => ReservedWords::For,
            "while" => ReservedWords::While,
            "false" => ReservedWords::False,
            "true" => ReservedWords::True,
            "class" => ReservedWords::Class,
            "super" => ReservedWords::Super,
            "this" => ReservedWords::This,
            "fun" => ReservedWords::Fun,
            "return" => ReservedWords::Return,
            "print" => ReservedWords::Print,
            "nil" => ReservedWords::Nil,
            "var" => ReservedWords::Var,
            _ => panic!("invalid reserved word"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TwoChars {
    BangEqual,
    EqualEqual,
    GreaterEqual,
    LessEqual,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Operators {
    Minus,
    Plus,
    Slash,
    Star,
    Bang,
    Equal,
    Greater,
    Less,
    TwoChars(TwoChars),
}

impl From<Operators> for &[u8] {
    fn from(value: Operators) -> Self {
        match value {
            Operators::Minus => b"-",
            Operators::Plus => b"+",
            Operators::Slash => b"/",
            Operators::Star => b"*",
            Operators::Bang => b"!",
            Operators::Equal => b"=",
            Operators::Greater => b">",
            Operators::Less => b"<",
            Operators::TwoChars(t) => match t {
                TwoChars::BangEqual => b"!=",
                TwoChars::EqualEqual => b"==",
                TwoChars::GreaterEqual => b">=",
                TwoChars::LessEqual => b"<=",
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenTypes {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Semicolon,
    Operator(Operators),
    Identifier,
    ReservedWord(ReservedWords),
    String,
    Number,
    Eof,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal<'string> {
    String(&'string str),
    Integer(u64),
    Float(f64),
}

impl<'string> fmt::Display for Literal<'string> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::String(s) => s.to_string(),
                Self::Integer(i) => i.to_string(),
                Self::Float(f) => f.to_string(),
            }
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Lexeme<'string> {
    Literal(Literal<'string>),
    Word(&'string str),
    Char(char),
    Eof,
}

impl<'string> fmt::Display for Lexeme<'string> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Lexeme::Literal(l) => l.to_string(),
                Lexeme::Word(s) => s.to_string(),
                Lexeme::Char(c) => c.to_string(),
                Lexeme::Eof => "".to_string(),
            }
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token<'lexeme> {
    pub(crate) token_type: TokenTypes,
    pub(crate) lexeme: Lexeme<'lexeme>,
    line: usize,
}

impl<'lexeme> Token<'lexeme> {
    pub fn new(token_type: TokenTypes, lexeme: Lexeme<'lexeme>, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}

impl<'lexeme> std::fmt::Display for Token<'lexeme> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

mod tests {
    #[test]
    fn from_str() {
        use super::*;

        let w_s = "while";
        let w_t: TokenTypes = TokenTypes::ReservedWord(w_s.into());

        assert_eq!(w_t, TokenTypes::ReservedWord(ReservedWords::While));
    }
}
