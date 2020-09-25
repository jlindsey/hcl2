use super::{Node, Operator, Result, Span, Token};
use std::convert::TryFrom;

use nom::{branch::alt, bytes::complete::tag, error::ErrorKind, Err};
use nom_tracable::tracable_parser;

#[tracable_parser]
pub(super) fn sign(i: Span) -> Result {
    let (i, span) = alt((tag("-"), tag("+")))(i)?;
    Operator::try_from(*span.fragment())
        .map(|op| (i, Node::new(Token::Operator(op), &span)))
        .map_err(|_| Err::Error((span, ErrorKind::ParseTo)))
}

#[tracable_parser]
pub(super) fn unary_operation(i: Span) -> Result {
    todo!()
}

#[tracable_parser]
pub(super) fn binary_operation(i: Span) -> Result {
    todo!()
}

#[tracable_parser]
pub(super) fn operation(i: Span) -> Result {
    alt((unary_operation, binary_operation))(i)
}
