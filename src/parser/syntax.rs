use crate::{
    ast::Expressions,
    token::{Lexeme, Operators, Token, TokenTypes, TwoChars},
};

use super::combinators::{either, or, pair, token, triple, zero_or_more, OrResult, Parser};

pub struct SyntaxParser<'tokens> {
    tokens: Vec<Token<'tokens>>,
}

impl<'tokens> SyntaxParser<'tokens> {
    pub fn new(tokens: Vec<Token<'tokens>>) -> SyntaxParser<'tokens> {
        SyntaxParser { tokens }
    }

    pub fn parse(&'tokens mut self) -> Expressions<'tokens> {
        expr().parse(&self.tokens).unwrap().0
    }
}

fn grouping<'tokens>() -> impl Parser<'tokens, Expressions<'tokens>> {
    move |tokens: &'tokens [Token<'tokens>]| {
        let left_par = token(TokenTypes::LeftParen);
        let right_par = token(TokenTypes::RightParen);
        let e = expr();

        let p = triple(&left_par, &e, &right_par).parse(tokens).map(
            |((_left_par, expr, _right_par), end)| {
                (
                    Expressions::Grouping {
                        expr: Box::new(expr),
                    },
                    end,
                )
            },
        );

        p
    }
}

fn operators<'tokens>(operators: Vec<Operators>) -> impl Parser<'tokens, Token<'tokens>> {
    move |tokens: &'tokens [Token<'tokens>]| {
        let parsers = operators
            .iter()
            .map(|op| token(TokenTypes::Operator(*op)))
            .collect();

        let p = either(parsers).parse(tokens);

        p
    }
}

macro_rules! create_repeat_rule {
    ($rule_name:ident, $inner_rule:ident, $( $operator:expr ),* ) => {
        fn $rule_name<'tokens>() -> impl Parser<'tokens, Expressions<'tokens>> {
            move |tokens: &'tokens [Token<'tokens>]| {
                let head = $inner_rule();
                let operators = operators(vec![
                    $(
                        $operator,
                    )*
                ]);
                let tail = pair(&operators, &head);
                let tails = zero_or_more(&tail);
                let p = pair(&head, &tails)
                    .parse(tokens)
                    .map(|((head, factors), end)| {
                        if factors.is_empty() {
                            (head, end)
                        } else {
                            let mut factors = factors.iter();
                            let (first_operator, first_factor) =
                                factors.next().expect("factors should not be empty");
                            let mut expr = Expressions::Binary {
                                left: Box::new(head),
                                operator: *first_operator,
                                right: Box::new(first_factor.clone()),
                            };

                            for (operator, factor) in factors {
                                expr = Expressions::Binary {
                                    left: Box::new(expr),
                                    operator: *operator,
                                    right: Box::new(factor.clone()),
                                };
                            }

                            (expr, end)
                        }
                    });

                p
            }
        }
    };
}

fn primary<'tokens>() -> impl Parser<'tokens, Expressions<'tokens>> {
    move |tokens: &'tokens [Token<'tokens>]| {
        let primary = either(vec![token(TokenTypes::Number), token(TokenTypes::String)]);
        let group = grouping();
        let p = or(&primary, &group)
            .parse(tokens)
            .and_then(|(result, next)| match result {
                OrResult::First(token) => {
                    if let Lexeme::Literal(l) = token.lexeme {
                        Ok((Expressions::Literal(l), next))
                    } else {
                        Err("unexpected lexeme")
                    }
                }
                OrResult::Second(group) => Ok((group, next)),
            });

        p
    }
}

fn unary<'tokens>() -> impl Parser<'tokens, Expressions<'tokens>> {
    move |tokens: &'tokens [Token<'tokens>]| {
        let operators = operators(vec![Operators::Bang, Operators::Minus]);
        let lit = primary();
        let un = unary();
        let pair = pair(&operators, &un);

        let p = or(&pair, &lit)
            .parse(tokens)
            .map(|(result, end)| match result {
                OrResult::First((operator, unary)) => (
                    Expressions::Unary {
                        operator,
                        right: Box::new(unary),
                    },
                    end,
                ),
                OrResult::Second(primary) => (primary, end),
            });

        p
    }
}

create_repeat_rule!(factor, unary, Operators::Slash, Operators::Star);

create_repeat_rule!(term, factor, Operators::Minus, Operators::Plus);

create_repeat_rule!(
    comparison,
    term,
    Operators::Greater,
    Operators::Less,
    Operators::TwoChars(TwoChars::GreaterEqual),
    Operators::TwoChars(TwoChars::LessEqual)
);

create_repeat_rule!(
    equality,
    comparison,
    Operators::TwoChars(TwoChars::BangEqual),
    Operators::TwoChars(TwoChars::EqualEqual)
);

fn expr<'tokens>() -> impl Parser<'tokens, Expressions<'tokens>> {
    move |tokens: &'tokens [Token<'tokens>]| equality().parse(tokens)
}

mod tests {
    #[test]
    fn parse_grouping() {
        use super::{grouping, Parser};
        use crate::{
            ast::Expressions,
            token::{Lexeme, Literal, Token, TokenTypes},
        };

        let list = [
            Token::new(TokenTypes::LeftParen, Lexeme::Char('('), 1),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(12)), 1),
            Token::new(TokenTypes::RightParen, Lexeme::Char(')'), 1),
        ];
        let parser = grouping();

        assert_eq!(
            Ok((
                Expressions::Grouping {
                    expr: Box::new(Expressions::Literal(Literal::Integer(12)))
                },
                [].as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn parse_operators() {
        use super::{operators, Parser};
        use crate::token::{Lexeme, Operators, Token, TokenTypes};

        let list = [
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
            Token::new(TokenTypes::Operator(Operators::Slash), Lexeme::Char('/'), 1),
        ];
        let parser = operators(vec![Operators::Minus, Operators::Plus]);

        assert_eq!(
            Ok((
                Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
                [Token::new(
                    TokenTypes::Operator(Operators::Slash),
                    Lexeme::Char('/'),
                    1
                ),]
                .as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn parse_primary() {
        use super::{primary, Parser};
        use crate::{
            ast::Expressions,
            token::{Lexeme, Literal, Operators, Token, TokenTypes},
        };

        let list = [
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(12)), 1),
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
        ];
        let parser = primary();

        assert_eq!(
            Ok((
                Expressions::Literal(Literal::Integer(12)),
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

    #[test]
    fn parse_unary() {
        use super::{unary, Parser};
        use crate::{
            ast::Expressions,
            token::{Lexeme, Literal, Operators, Token, TokenTypes},
        };

        let list = [
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(12)), 1),
        ];
        let parser = unary();

        assert_eq!(
            Ok((
                Expressions::Unary {
                    operator: Token::new(
                        TokenTypes::Operator(Operators::Minus),
                        Lexeme::Char('-'),
                        1
                    ),
                    right: Box::new(Expressions::Literal(Literal::Integer(12)))
                },
                [].as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn parse_factor_simple() {
        use super::{factor, Parser};
        use crate::{
            ast::Expressions,
            token::{Lexeme, Literal, Operators, Token, TokenTypes},
        };

        let list = [
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(12)), 1),
            Token::new(TokenTypes::Operator(Operators::Star), Lexeme::Char('*'), 1),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(34)), 1),
        ];
        let parser = factor();

        assert_eq!(
            Ok((
                Expressions::Binary {
                    left: Box::new(Expressions::Literal(Literal::Integer(12))),
                    operator: Token::new(
                        TokenTypes::Operator(Operators::Star),
                        Lexeme::Char('*'),
                        1
                    ),
                    right: Box::new(Expressions::Literal(Literal::Integer(34)))
                },
                [].as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn parse_factor_multiple() {
        use super::{factor, Parser};
        use crate::{
            ast::Expressions,
            token::{Lexeme, Literal, Operators, Token, TokenTypes},
        };

        let list = [
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(12)), 1),
            Token::new(TokenTypes::Operator(Operators::Star), Lexeme::Char('*'), 1),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(34)), 1),
            Token::new(TokenTypes::Operator(Operators::Star), Lexeme::Char('*'), 1),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(56)), 1),
            Token::new(TokenTypes::Operator(Operators::Star), Lexeme::Char('*'), 1),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(78)), 1),
        ];
        let parser = factor();

        let first = Expressions::Binary {
            left: Box::new(Expressions::Literal(Literal::Integer(12))),
            operator: Token::new(TokenTypes::Operator(Operators::Star), Lexeme::Char('*'), 1),
            right: Box::new(Expressions::Literal(Literal::Integer(34))),
        };
        let second = Expressions::Binary {
            left: Box::new(first),
            operator: Token::new(TokenTypes::Operator(Operators::Star), Lexeme::Char('*'), 1),
            right: Box::new(Expressions::Literal(Literal::Integer(56))),
        };
        let third = Expressions::Binary {
            left: Box::new(second),
            operator: Token::new(TokenTypes::Operator(Operators::Star), Lexeme::Char('*'), 1),
            right: Box::new(Expressions::Literal(Literal::Integer(78))),
        };

        assert_eq!(Ok((third, [].as_slice())), parser.parse(&list),);
    }

    #[test]
    fn parse_term() {
        use super::{term, Parser};
        use crate::{
            ast::Expressions,
            token::{Lexeme, Literal, Operators, Token, TokenTypes},
        };

        let list = [
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(12)), 1),
            Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(34)), 1),
        ];
        let parser = term();

        assert_eq!(
            Ok((
                Expressions::Binary {
                    left: Box::new(Expressions::Literal(Literal::Integer(12))),
                    operator: Token::new(
                        TokenTypes::Operator(Operators::Plus),
                        Lexeme::Char('+'),
                        1
                    ),
                    right: Box::new(Expressions::Literal(Literal::Integer(34)))
                },
                [].as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn parse_comparison() {
        use super::{comparison, Parser};
        use crate::{
            ast::Expressions,
            token::{Lexeme, Literal, Operators, Token, TokenTypes},
        };

        let list = [
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(12)), 1),
            Token::new(
                TokenTypes::Operator(Operators::Greater),
                Lexeme::Char('>'),
                1,
            ),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(34)), 1),
        ];
        let parser = comparison();

        assert_eq!(
            Ok((
                Expressions::Binary {
                    left: Box::new(Expressions::Literal(Literal::Integer(12))),
                    operator: Token::new(
                        TokenTypes::Operator(Operators::Greater),
                        Lexeme::Char('>'),
                        1
                    ),
                    right: Box::new(Expressions::Literal(Literal::Integer(34)))
                },
                [].as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn parse_equality() {
        use super::{equality, Parser};
        use crate::{
            ast::Expressions,
            token::{Lexeme, Literal, Operators, Token, TokenTypes, TwoChars},
        };

        let list = [
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(12)), 1),
            Token::new(
                TokenTypes::Operator(Operators::TwoChars(TwoChars::BangEqual)),
                Lexeme::Word("!="),
                1,
            ),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(34)), 1),
        ];
        let parser = equality();

        assert_eq!(
            Ok((
                Expressions::Binary {
                    left: Box::new(Expressions::Literal(Literal::Integer(12))),
                    operator: Token::new(
                        TokenTypes::Operator(Operators::TwoChars(TwoChars::BangEqual)),
                        Lexeme::Word("!="),
                        1
                    ),
                    right: Box::new(Expressions::Literal(Literal::Integer(34)))
                },
                [].as_slice()
            )),
            parser.parse(&list),
        );
    }

    #[test]
    fn check_precedence() {
        use super::{term, Parser};
        use crate::{
            ast::Expressions,
            token::{Lexeme, Literal, Operators, Token, TokenTypes},
        };

        let list = [
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(12)), 1),
            Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(34)), 1),
            Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(56)), 1),
            Token::new(TokenTypes::Operator(Operators::Star), Lexeme::Char('*'), 1),
            Token::new(TokenTypes::Number, Lexeme::Literal(Literal::Integer(78)), 1),
        ];
        let parser = term();

        let mul = Expressions::Binary {
            left: Box::new(Expressions::Literal(Literal::Integer(56))),
            operator: Token::new(TokenTypes::Operator(Operators::Star), Lexeme::Char('*'), 1),
            right: Box::new(Expressions::Literal(Literal::Integer(78))),
        };
        let sub = Expressions::Binary {
            left: Box::new(Expressions::Literal(Literal::Integer(12))),
            operator: Token::new(TokenTypes::Operator(Operators::Minus), Lexeme::Char('-'), 1),
            right: Box::new(Expressions::Literal(Literal::Integer(34))),
        };
        let add = Expressions::Binary {
            left: Box::new(sub),
            operator: Token::new(TokenTypes::Operator(Operators::Plus), Lexeme::Char('+'), 1),
            right: Box::new(mul),
        };

        assert_eq!(Ok((add, [].as_slice())), parser.parse(&list),);
    }
}
