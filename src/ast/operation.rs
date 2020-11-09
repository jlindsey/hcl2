use super::{expr_term, BinaryOp, Node, Operator, Result, Span, Token, UnaryOp};
use std::rc::Rc;

use nom::{
    branch::alt,
    bytes::complete::{is_a, tag},
    character::complete::{anychar, char, space0},
    combinator::map,
    error::ErrorKind,
    sequence::tuple,
    Err,
};
use nom_locate::position;
use nom_tracable::tracable_parser;

#[tracable_parser]
pub(super) fn sign(i: Span) -> Result {
    let (i, start) = position(i)?;
    map(alt((char('-'), char('+'))), move |c: char| {
        let op = if c == '-' {
            Operator::Minus
        } else {
            Operator::Plus
        };
        Node::new(Token::Operator(op), &start)
    })(i)
}

#[tracable_parser]
fn negation(i: Span) -> Result {
    let (i, start) = position(i)?;
    map(char('!'), move |_| {
        Node::new(Token::Operator(Operator::Not), &start)
    })(i)
}

#[tracable_parser]
fn unary_operator(i: Span) -> Result {
    alt((sign, negation))(i)
}

#[tracable_parser]
fn arithmetic_operator(i: Span) -> Result {
    let (i, start) = position(i)?;
    let (i, c) = anychar(i)?;

    let op = match c {
        '+' => Ok(Operator::Plus),
        '-' => Ok(Operator::Minus),
        '*' => Ok(Operator::Multiply),
        '/' => Ok(Operator::Divide),
        '%' => Ok(Operator::Modulus),
        _ => Err(Err::Error((i, ErrorKind::Char))),
    }?;

    Ok((i, Node::new(Token::Operator(op), &start)))
}

#[tracable_parser]
fn logic_operator(i: Span) -> Result {
    map(alt((tag("&&"), tag("||"))), move |span: Span| {
        let op = if *span.fragment() == "&&" {
            Operator::And
        } else {
            Operator::Or
        };
        Node::new(Token::Operator(op), &span)
    })(i)
}

#[tracable_parser]
fn comparison_operator(i: Span) -> Result {
    let (i, span) = is_a("=!><")(i)?;
    let op = match *span.fragment() {
        "==" => Ok(Operator::Equal),
        "!=" => Ok(Operator::NotEqual),
        "<" => Ok(Operator::Less),
        ">" => Ok(Operator::Greater),
        "<=" => Ok(Operator::LessEqual),
        ">=" => Ok(Operator::GreaterEqual),
        _ => Err(Err::Error((i, ErrorKind::IsA))),
    }?;

    Ok((i, Node::new(Token::Operator(op), &span)))
}

#[tracable_parser]
pub(super) fn binary_operator(i: Span) -> Result {
    alt((arithmetic_operator, comparison_operator, logic_operator))(i)
}

pub(super) fn unary_operation(i: Span) -> Result {
    map(
        tuple((unary_operator, space0, expr_term)),
        move |(op, _, expr)| {
            let op = Rc::new(op);
            let unop = UnaryOp {
                operator: Rc::clone(&op),
                operand: Rc::new(expr),
            };

            Node::from_node(Token::UnaryOp(unop), &op)
        },
    )(i)
}

#[tracable_parser]
pub(super) fn binary_operation(i: Span) -> Result {
    map(
        tuple((expr_term, space0, binary_operator, space0, expr_term)),
        move |(left, _, op, _, right)| {
            let left = Rc::new(left);
            let binop = BinaryOp {
                operator: Rc::new(op),
                left: Rc::clone(&left),
                right: Rc::new(right),
            };

            Node::from_node(Token::BinaryOp(binop), &left)
        },
    )(i)
}

#[tracable_parser]
pub(super) fn operation(i: Span) -> Result {
    alt((unary_operation, binary_operation))(i)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::test::{info, Result};
    use std::convert::TryFrom;

    use nom_tracable::TracableInfo;
    use rstest::rstest;

    #[rstest(input, expected,
        case("!foo", node!(unary_op!("!", ident!("foo")))),
        case("-test", node!(unary_op!("-", ident!("test"))))
    )]
    fn test_unary_op(input: &'static str, expected: Node, info: TracableInfo) -> Result {
        let span = Span::new_extra(input, info);
        let (span, node) = unary_operation(span)?;
        assert!(span.fragment().is_empty());

        node.assert_same_token(&expected);

        Ok(())
    }
}
