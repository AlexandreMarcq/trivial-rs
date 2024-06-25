use crate::token::{Token, TokenTypes};

#[derive(Debug, PartialEq)]
pub enum OrResult<O1, O2> {
    First(O1),
    Second(O2),
}

type ParserResult<'tokens, T> = Result<(T, &'tokens [Token<'tokens>]), &'static str>;

pub trait Parser<'tokens, T> {
    fn parse(&self, input: &'tokens [Token<'tokens>]) -> ParserResult<'tokens, T>;
}

impl<'tokens, F, T> Parser<'tokens, T> for F
where
    F: Fn(&'tokens [Token<'tokens>]) -> ParserResult<'tokens, T>,
{
    fn parse(&self, input: &'tokens [Token<'tokens>]) -> ParserResult<'tokens, T> {
        self(input)
    }
}

pub fn token<'tokens, 'closure>(
    expected: TokenTypes,
) -> impl Parser<'tokens, Token<'tokens>> + 'closure {
    move |tokens: &'tokens [Token<'tokens>]| match tokens.get(0) {
        Some(token) if token.token_type == expected => Ok((*token, &tokens[1..])),
        _ => Err("unexpected token"),
    }
}

pub fn pair<'tokens, 'parser, P1, P2, R1, R2>(
    parser1: &'parser P1,
    parser2: &'parser P2,
) -> impl Parser<'tokens, (R1, R2)> + 'parser
where
    P1: Parser<'tokens, R1>,
    P2: Parser<'tokens, R2>,
{
    move |tokens: &'tokens [Token<'tokens>]| {
        parser1.parse(tokens).and_then(|(result1, next1)| {
            parser2
                .parse(next1)
                .map(|(result2, next2)| ((result1, result2), next2))
        })
    }
}

pub fn triple<'tokens, 'parser, P1, P2, P3, R1, R2, R3>(
    parser1: &'parser P1,
    parser2: &'parser P2,
    parser3: &'parser P3,
) -> impl Parser<'tokens, (R1, R2, R3)> + 'parser
where
    P1: Parser<'tokens, R1>,
    P2: Parser<'tokens, R2>,
    P3: Parser<'tokens, R3>,
{
    move |tokens: &'tokens [Token<'tokens>]| {
        parser1.parse(tokens).and_then(|(result1, next1)| {
            parser2.parse(next1).and_then(|(result2, next2)| {
                parser3
                    .parse(next2)
                    .map(|(result3, end)| ((result1, result2, result3), end))
            })
        })
    }
}

pub fn or<'tokens, 'parser, P1, P2, R1, R2>(
    parser1: &'parser P1,
    parser2: &'parser P2,
) -> impl Parser<'tokens, OrResult<R1, R2>> + 'parser
where
    P1: Parser<'tokens, R1>,
    P2: Parser<'tokens, R2>,
{
    move |tokens: &'tokens [Token<'tokens>]| match parser1.parse(tokens) {
        Ok((result, end)) => Ok((OrResult::First(result), end)),
        Err(_) => parser2
            .parse(tokens)
            .map(|(result, end)| (OrResult::Second(result), end)),
    }
}

pub fn zero_or_more<'tokens, 'parser, P, T>(
    parser: &'parser P,
) -> impl Parser<'tokens, Vec<T>> + 'parser
where
    P: Parser<'tokens, T>,
{
    move |mut tokens: &'tokens [Token<'tokens>]| {
        let mut results = vec![];

        while let Ok((result, next)) = parser.parse(tokens) {
            results.push(result);
            tokens = next;
        }

        Ok((results, tokens))
    }
}

pub fn one_or_more<'tokens, 'parser, P, T>(
    parser: &'parser P,
) -> impl Parser<'tokens, Vec<T>> + 'parser
where
    P: Parser<'tokens, T>,
{
    move |mut tokens: &'tokens [Token<'tokens>]| {
        let mut results = vec![];

        if let Ok((result, next)) = parser.parse(tokens) {
            results.push(result);
            tokens = next;
        } else {
            return Err("unexpected token");
        }

        while let Ok((result, next)) = parser.parse(tokens) {
            results.push(result);
            tokens = next;
        }

        Ok((results, tokens))
    }
}

pub fn either<'tokens, P, T>(parsers: Vec<P>) -> impl Parser<'tokens, T>
where
    P: Parser<'tokens, T>,
{
    move |tokens: &'tokens [Token<'tokens>]| {
        for parser in &parsers {
            if let Ok(v) = parser.parse(tokens) {
                return Ok(v);
            }
        }
        Err("unexpected token")
    }
}

mod tests {
    #[test]
    fn parse_token() {
        use super::{token, Parser};
        use crate::token::{Lexeme, Operators, Token, TokenTypes};

        let list = [
            Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
        ];
        let plus = token(TokenTypes::Operator(Operators::Plus));

        assert_eq!(
            Ok((
                (Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1)),
                [Token::new(
                    TokenTypes::Operator(Operators::Minus),
                    Lexeme::Char('-'),
                    1
                )]
                .as_slice()
            )),
            plus.parse(&list)
        );
    }

    #[test]
    fn parse_pair() {
        use super::{pair, token, Parser};
        use crate::token::{Lexeme, Operators, Token, TokenTypes};

        let list = [
            Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
        ];
        let plus = token(TokenTypes::Operator(Operators::Plus));
        let parser = pair(&plus, &plus);

        assert_eq!(
            Ok((
                (
                    Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
                    Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1)
                ),
                [Token::new(
                    TokenTypes::Operator(Operators::Minus),
                    Lexeme::Char('-'),
                    1
                ),]
                .as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn parse_triple() {
        use super::{token, triple, Parser};
        use crate::token::{Lexeme, Operators, Token, TokenTypes};

        let list = [
            Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
            Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            Token::new(TokenTypes::Operator(Operators::Star), Lexeme::Char('*'), 1),
        ];
        let plus = token(TokenTypes::Operator(Operators::Plus));
        let minus = token(TokenTypes::Operator(Operators::Minus));
        let parser = triple(&plus, &minus, &plus);

        assert_eq!(
            Ok((
                (
                    Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
                    Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
                    Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1)
                ),
                [Token::new(
                    TokenTypes::Operator(Operators::Star),
                    Lexeme::Char('*'),
                    1
                ),]
                .as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn parse_or() {
        use super::{or, token, OrResult, Parser};
        use crate::token::{Lexeme, Operators, Token, TokenTypes};

        let list = [
            Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
        ];
        let plus = token(TokenTypes::Operator(Operators::Plus));
        let slash = token(TokenTypes::Operator(Operators::Slash));
        let parser = or(&plus, &slash);

        assert_eq!(
            Ok((
                OrResult::First(Token::new(
                    TokenTypes::Operator(Operators::Plus),
                    Lexeme::Char('+'),
                    1
                )),
                [Token::new(
                    TokenTypes::Operator(Operators::Minus),
                    Lexeme::Char('-'),
                    1
                ),]
                .as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn parse_zero_or_more() {
        use super::{token, zero_or_more, Parser};
        use crate::token::{Lexeme, Operators, Token, TokenTypes};

        let list = [
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
        ];
        let plus = token(TokenTypes::Operator(Operators::Plus));
        let parser = zero_or_more(&plus);

        assert_eq!(
            Ok((
                vec![],
                [
                    Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
                    Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
                ]
                .as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn parse_one_or_more() {
        use super::{one_or_more, token, Parser};
        use crate::token::{Lexeme, Operators, Token, TokenTypes};

        let list = [
            Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
        ];
        let plus = token(TokenTypes::Operator(Operators::Plus));
        let parser = one_or_more(&plus);

        assert_eq!(
            Ok((
                vec![
                    Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
                    Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1)
                ],
                [Token::new(
                    TokenTypes::Operator(Operators::Minus),
                    Lexeme::Char('-'),
                    1
                ),]
                .as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn parse_choice() {
        use super::{either, token, Parser};
        use crate::token::{Lexeme, Operators, Token, TokenTypes};

        let list = [
            Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
        ];
        let plus = token(TokenTypes::Operator(Operators::Plus));
        let minus = token(TokenTypes::Operator(Operators::Minus));
        let parser = either(vec![plus, minus]);

        assert_eq!(
            Ok((
                Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
                [Token::new(
                    TokenTypes::Operator(Operators::Minus),
                    Lexeme::Char('-'),
                    1
                )]
                .as_slice()
            )),
            parser.parse(&list),
        );
    }
}
