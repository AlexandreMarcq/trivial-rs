use std::io::Write;

use crate::token::{Lexeme, Literal, Operators, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum Expressions<'value> {
    Literal(Literal<'value>),
    Unary {
        operator: Token<'value>,
        right: Box<Expressions<'value>>,
    },
    Binary {
        left: Box<Expressions<'value>>,
        operator: Token<'value>,
        right: Box<Expressions<'value>>,
    },
    Grouping {
        expr: Box<Expressions<'value>>,
    },
}

impl<'value> Expressions<'value> {
    fn print(&self, buffer: &mut String, prefix: &str, sub_prefix: &str) {
        buffer.push_str(prefix);
        match self {
            Expressions::Literal(literal) => {
                buffer.push_str(format!("{literal}").as_str());
                buffer.push('\n');
            }
            Expressions::Unary { operator, right } => {
                if let Lexeme::Char(c) = operator.lexeme {
                    buffer.push(c);
                } else {
                    buffer.push('#');
                }
                buffer.push('\n');
                right.print(
                    buffer,
                    format!("{sub_prefix}└─ ").as_str(),
                    format!("{sub_prefix}   ").as_str(),
                );
            }
            Expressions::Binary {
                left,
                operator,
                right,
            } => {
                match operator.lexeme {
                    Lexeme::Char(c) => buffer.push(c),
                    Lexeme::Word(w) => buffer.push_str(w),
                    _ => buffer.push('#'),
                }
                buffer.push('\n');
                left.print(
                    buffer,
                    format!("{sub_prefix}├─ ").as_str(),
                    format!("{sub_prefix}│  ").as_str(),
                );
                right.print(
                    buffer,
                    format!("{sub_prefix}└─ ").as_str(),
                    format!("{sub_prefix}   ").as_str(),
                );
            }
            Expressions::Grouping { expr } => {
                buffer.push_str("(\n");
                expr.print(
                    buffer,
                    format!("{sub_prefix}├─ ").as_str(),
                    format!("{sub_prefix}│  ").as_str(),
                );
                buffer.push_str(format!("{sub_prefix}) \n").as_str());
            }
        }
    }
}

impl<'value> std::fmt::Display for Expressions<'value> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut buffer = String::new();
        self.print(&mut buffer, "", "");
        write!(f, "{buffer}")
    }
}

pub trait Expr {
    fn print<W: Write>(&self, out: &mut W);
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LiteralExpr<'value> {
    value: Literal<'value>,
}

impl<'value> LiteralExpr<'value> {
    pub fn new(value: Literal<'value>) -> LiteralExpr<'value> {
        LiteralExpr { value }
    }
}

impl<'value> Expr for LiteralExpr<'value> {
    fn print<W: Write>(&self, out: &mut W) {
        write!(out, "{}", self.value).expect("could not write literal expression");
    }
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr<E> {
    operator: Operators,
    right: E,
}

impl<E> UnaryExpr<E> {
    pub fn new(operator: Operators, right: E) -> Self {
        Self { operator, right }
    }
}

impl<E: Expr> Expr for UnaryExpr<E> {
    fn print<W: Write>(&self, out: &mut W) {
        out.write(b"(")
            .expect("could not write grouping expression");
        out.write(self.operator.into())
            .expect("could not write unary expression");
        out.write(b" ")
            .expect("could not write grouping expression");
        self.right.print(out);
        out.write(b")")
            .expect("could not write grouping expression");
    }
}

pub struct BinaryExpr<E1, E2> {
    left: E1,
    operator: Operators,
    right: E2,
}

impl<E1, E2> BinaryExpr<E1, E2> {
    pub fn new(left: E1, operator: Operators, right: E2) -> BinaryExpr<E1, E2> {
        BinaryExpr {
            left,
            operator,
            right,
        }
    }
}

impl<'a, E1, E2> Expr for BinaryExpr<E1, E2>
where
    E1: Expr,
    E2: Expr,
{
    fn print<W: Write>(&self, out: &mut W) {
        out.write(b"(").expect("could not write binary expression");
        out.write(self.operator.into())
            .expect("could not write binary expression");
        out.write(b" ").expect("could not write binary expression");
        self.left.print(out);
        out.write(b" ").expect("could not write binary expression");
        self.right.print(out);
        out.write(b")").expect("could not write binary expression");
    }
}

pub struct GroupingExpr<E> {
    expr: E,
}

impl<E> GroupingExpr<E> {
    pub fn new(expr: E) -> Self {
        Self { expr }
    }
}

impl<E: Expr> Expr for GroupingExpr<E> {
    fn print<W: Write>(&self, out: &mut W) {
        out.write(b"(group ")
            .expect("could not write grouping expression");
        self.expr.print(out);
        out.write(b")")
            .expect("could not write grouping expression");
    }
}

mod tests {
    #[test]
    fn print_literal() {
        use super::{Expr, LiteralExpr};
        use crate::token::Literal;

        let literal = LiteralExpr::new(Literal::Integer(12));
        let mut output = Vec::new();

        literal.print(&mut output);

        let output = String::from_utf8(output).expect("not UTF-8");

        assert_eq!(output, "12");
    }

    #[test]
    fn print_binary() {
        use super::{BinaryExpr, Expr, LiteralExpr};
        use crate::token::{Literal, Operators};

        let left = LiteralExpr::new(Literal::Integer(1));
        let right = LiteralExpr::new(Literal::Integer(2));
        let operator = Operators::Plus;
        let binary = BinaryExpr::new(left, operator, right);
        let mut output = Vec::new();

        binary.print(&mut output);

        let output = String::from_utf8(output).expect("not UTF-8");

        assert_eq!(output, "(+ 1 2)");
    }

    #[test]
    fn print_grouping() {
        use super::{Expr, GroupingExpr, LiteralExpr};
        use crate::token::Literal;

        let expr = LiteralExpr::new(Literal::Integer(12));
        let grouping = GroupingExpr::new(expr);
        let mut output = Vec::new();

        grouping.print(&mut output);

        let output = String::from_utf8(output).expect("not UTF-8");

        assert_eq!(output, "(group 12)");
    }

    #[test]
    fn print_unary() {
        use super::{Expr, LiteralExpr, UnaryExpr};
        use crate::token::{Literal, Operators};

        let right = LiteralExpr::new(Literal::Float(1.2));
        let operator = Operators::Minus;
        let unary = UnaryExpr::new(operator, right);
        let mut output = Vec::new();

        unary.print(&mut output);

        let output = String::from_utf8(output).expect("not UTF-8");

        assert_eq!(output, "(- 1.2)");
    }

    #[test]
    fn print_tree() {
        use super::*;
        use crate::token::{Literal, Operators};

        let expr = BinaryExpr::new(
            UnaryExpr::new(Operators::Minus, LiteralExpr::new(Literal::Integer(123))),
            Operators::Star,
            GroupingExpr::new(LiteralExpr::new(Literal::Float(45.67))),
        );
        let mut output = Vec::new();

        expr.print(&mut output);

        let output = String::from_utf8(output).expect("not UTF-8");

        assert_eq!(output, "(* (- 123) (group 45.67))");
    }
}
