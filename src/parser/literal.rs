use super::{identifier, number, Heredoc, Node, Result, Span, Token};
use std::rc::Rc;

use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag},
    character::complete::{anychar, char, newline, one_of, space0},
    combinator::{map, peek},
    error::ErrorKind,
    multi::many_till,
    sequence::{delimited, tuple},
    Err,
};
use nom_tracable::tracable_parser;

#[tracable_parser]
fn null_literal(i: Span) -> Result {
    map(tag("null"), |span: Span| Node::new(Token::Null, &span))(i)
}

#[tracable_parser]
fn true_literal(i: Span) -> Result {
    map(tag("true"), |span: Span| Node::new(Token::True, &span))(i)
}

#[tracable_parser]
fn false_literal(i: Span) -> Result {
    map(tag("false"), |span: Span| Node::new(Token::False, &span))(i)
}

#[tracable_parser]
fn boolean(i: Span) -> Result {
    let (_, head): (_, char) = peek(anychar)(i)?;
    match head {
        't' => true_literal(i),
        'f' => false_literal(i),
        _ => Err(Err::Error((i, ErrorKind::Tag))),
    }
}
#[tracable_parser]
fn heredoc(i: Span) -> Result {
    let (i, (opening, ident, _)) = tuple((alt((tag("<<-"), tag("<<"))), identifier, newline))(i)?;
    let truncate = *opening.fragment() == "<<-";

    let (i, content) = map(
        many_till(
            anychar,
            tuple((space0, tag(ident.token.as_identifier().unwrap()), newline)),
        ),
        |(chs, _)| chs.into_iter().collect::<String>(),
    )(i)?;

    let ident = Rc::new(ident);
    let heredoc = Heredoc {
        ident,
        truncate,
        content,
    };

    Ok((i, Node::new(Token::Heredoc(heredoc), &opening)))
}

#[tracable_parser]
pub(super) fn single_line_string(i: Span) -> Result {
    map(
        delimited(
            char('"'),
            escaped(is_not("\\\"\n"), '\\', one_of(r#"rnt"\"#)),
            char('"'),
        ),
        |span: Span| Node::new(Token::String(String::from(*span.fragment())), &span),
    )(i)
}

#[tracable_parser]
pub(super) fn string(i: Span) -> Result {
    let (_, head): (_, char) = peek(anychar)(i)?;
    match head {
        '"' => single_line_string(i),
        '<' => heredoc(i),
        _ => Err(Err::Error((i, ErrorKind::Char))),
    }
}
#[tracable_parser]
pub(super) fn literal(i: Span) -> Result {
    let (_, head): (_, char) = peek(anychar)(i)?;
    match head {
        'n' => null_literal(i),
        't' | 'f' => boolean(i),
        '"' | '<' => string(i),
        '-' | '0'..='9' => number(i),
        _ => Err(Err::Error((i, ErrorKind::Char))),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::test::{info, Result};
    use nom_tracable::TracableInfo;

    use rstest::rstest;

    #[rstest(input, expected,
        case("true", boolean!(true)),
        case("false", boolean!(false))
    )]
    fn test_boolean(input: &'static str, expected: Token, info: TracableInfo) -> Result {
        let i = Span::new_extra(input, info);
        let (span, node) = boolean(i)?;
        assert!(span.fragment().is_empty());

        assert_eq!(node.token, expected);

        let parsed: bool = input.parse()?;
        assert_eq!(node.token.as_boolean(), Some(parsed));

        Ok(())
    }

    #[rstest(input, expected,
        case(r#""hello there""#, string!("hello there")),
        case(r#""with numbers 1 2 3""#, string!("with numbers 1 2 3")),
        case(r#""escaped \"""#, string!("escaped \\\"")),
        case(r#""escaped \n""#, string!("escaped \\n"))
    )]
    fn test_single_line_string(input: &'static str, expected: Token, info: TracableInfo) -> Result {
        let input = Span::new_extra(input, info);
        let (span, node) = single_line_string(input)?;
        assert!(span.fragment().is_empty());

        assert_eq!(node.token, expected);

        Ok(())
    }

    #[rstest(input, expected,
        case(r#""single line string""#, node!(string!("single line string"))),
        case("<<EOF\na heredoc\nEOF\n", node!(heredoc!("EOF", false, "a heredoc\n"))),
        case(
            "<<-ENDO\na truncated heredoc\nENDO\n",
            node!(heredoc!("ENDO", true, "a truncated heredoc\n"))
        )
    )]
    fn test_strings(input: &'static str, expected: Node, info: TracableInfo) -> Result {
        let input = Span::new_extra(input, info);
        let (span, node) = string(input)?;
        assert!(span.fragment().is_empty());

        node.assert_same_token(&expected);

        Ok(())
    }

    #[rstest]
    fn test_non_trunc_heredoc(info: TracableInfo) {
        let test_str = "this is
        a test
        string
        in
        a
        heredoc\n";
        let input = format!("<<EOF\n{}EOF\n", test_str);
        let input = Span::new_extra(&input, info);
        let (span, node) = heredoc(input).unwrap();
        assert!(span.fragment().is_empty());

        let doc = node.token.as_heredoc().unwrap();
        assert_eq!(doc.ident.token, ident!("EOF"));
        assert!(!doc.truncate);
        assert_eq!(&doc.content, test_str);
    }

    #[rstest]
    fn test_trunc_heredoc(info: TracableInfo) {
        let test_str = "this is
        another test
        string
        in
        a
        heredoc
            but this time it is truncated!\n";
        let input = format!("<<-EOF\n{}EOF\n", test_str);
        let input = Span::new_extra(&input, info);
        let (span, node) = heredoc(input).unwrap();
        assert!(span.fragment().is_empty());

        let doc = node.token.as_heredoc().unwrap();
        assert_eq!(doc.ident.token, ident!("EOF"));
        assert!(doc.truncate);
        assert_eq!(&doc.content, test_str);
    }
}
