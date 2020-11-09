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
    use crate::ast::{
        test::{info, Result},
        Function, ObjectItem,
    };
    use std::convert::TryFrom;

    use nom_tracable::TracableInfo;
    use rstest::rstest;

    #[rstest(input, expected,
        case("!foo", node!(unary_op!("!", ident!("foo")))),
        case("-test", node!(unary_op!("-", ident!("test")))),
        case("!test_func(13)", node!(unary_op!("!", function!("test_func", number!(13))))),
        case("- 14", node!(unary_op!("-", number!(14)))),
        case("!true", node!(unary_op!("!", boolean!(true)))),
        case("![1, true, false]", node!(unary_op!("!", list!(number!(1), boolean!(true), boolean!(false)))))
    )]
    fn test_unary_op(input: &'static str, expected: Node, info: TracableInfo) -> Result {
        let span = Span::new_extra(input, info);
        let (span, node) = unary_operation(span)?;
        assert!(span.fragment().is_empty());

        node.assert_same_token(&expected);

        Ok(())
    }

    #[rstest(input, expected,
        case("1 > 2", node!(binary_op!(number!(1), ">", number!(2)))),
        case("2 < false", node!(binary_op!(number!(2), "<", boolean!(false)))),
        case("foo == bar", node!(binary_op!(ident!("foo"), "==", ident!("bar")))),
        case("baz != \"hello\"", node!(binary_op!(ident!("baz"), "!=", string!("hello")))),
        case("14.3 % 5", node!(binary_op!(number!(14.3), "%", number!(5)))),
        case("test * 7", node!(binary_op!(ident!("test"), "*", number!(7)))),
        case("17 - 73", node!(binary_op!(number!(17), "-", number!(73)))),
        case("bar + 18", node!(binary_op!(ident!("bar"), "+", number!(18)))),
        case("foo && bar", node!(binary_op!(ident!("foo"), "&&", ident!("bar")))),
        case("foo || bar", node!(binary_op!(ident!("foo"), "||", ident!("bar")))),
        case("foo && (bar || baz)", node!(
            binary_op!(
                ident!("foo"),
                "&&",
                 binary_op!(
                     ident!("bar"),
                     "||",
                     ident!("baz"))))),
        case(
            "{test: true} && {test: false}",
            node!(binary_op!(
                object!(ident!("test") => boolean!(true)),
                "&&",
                object!(ident!("test") => boolean!(false))
            ))
        )
    )]
    fn test_binary_op(input: &'static str, expected: Node, info: TracableInfo) -> Result {
        let span = Span::new_extra(input, info);
        let (span, node) = binary_operation(span)?;
        assert!(
            span.fragment().is_empty(),
            "expected fragment to be empty; got {}",
            span.fragment()
        );

        node.assert_same_token(&expected);

        Ok(())
    }
}
