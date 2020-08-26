#![allow(dead_code)]

use nom::{
    bytes::complete::tag,
    character::complete::{alphanumeric1, newline, space0},
    combinator::{map, opt},
    sequence::{delimited, separated_pair, terminated},
    IResult,
};

#[derive(Debug, PartialEq)]
pub struct Identifier(String);

pub enum Literal {
    Equal,
}

fn identifier(i: &str) -> IResult<&str, Identifier> {
    map(alphanumeric1, |ident| Identifier(String::from(ident)))(i)
}

fn string_literal(i: &str) -> IResult<&str, String> {
    map(delimited(tag("\""), alphanumeric1, tag("\"")), String::from)(i)
}

fn equal(i: &str) -> IResult<&str, Literal> {
    map(delimited(space0, tag("="), space0), |_| Literal::Equal)(i)
}

fn attribute(i: &str) -> IResult<&str, (Identifier, String)> {
    terminated(
        separated_pair(identifier, equal, string_literal),
        opt(newline),
    )(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error;

    #[test]
    fn parse_attribute() -> Result<(), Box<dyn Error>> {
        let res = attribute("foo = \"bar\"")?.1;
        assert_eq!(res.0, Identifier("foo".into()));
        assert_eq!(res.1, "bar".to_string());
        Ok(())
    }
}
