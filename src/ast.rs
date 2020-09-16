#![allow(dead_code)]

use std::{error::Error, rc::Rc};

mod tokens;
pub use tokens::*;

use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take_while, take_while_m_n},
    character::complete::{anychar, char, multispace0, newline, one_of, space0, space1},
    combinator::{complete, map, opt, peek},
    error::ErrorKind,
    multi::{fold_many1, separated_list},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    Err,
};
use nom_locate::LocatedSpan;
use nom_tracable::{tracable_parser, TracableInfo};

pub type Span<'a> = LocatedSpan<&'a str, TracableInfo>;

type Result<'a, I = Span<'a>, O = Node, E = (I, ErrorKind)> =
    std::result::Result<(I, O), nom::Err<E>>;
type OResult<'a> = std::result::Result<Tree, Box<dyn Error + 'a>>;

fn valid_ident_start_char(c: char) -> bool {
    c.is_alphabetic()
}

fn valid_ident_char(c: char) -> bool {
    c.is_alphanumeric() || matches!(c, '_' | '-')
}

#[tracable_parser]
fn identifier(i: Span) -> Result {
    map(
        tuple((
            take_while_m_n(1, 1, valid_ident_start_char),
            take_while(valid_ident_char),
        )),
        |(first, rest): (Span, Span)| {
            let mut m = String::from(*first.fragment());
            m.push_str(*rest.fragment());

            Node::new(Token::Identifier(m), &first)
        },
    )(i)
}

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
        _ => Err(Err::Failure((i, ErrorKind::Tag))),
    }
}

#[tracable_parser]
fn string(i: Span) -> Result {
    map(
        delimited(
            char('"'),
            escaped(is_not("\\\""), '\\', one_of(r#"rnt"\"#)),
            char('"'),
        ),
        |span: Span| Node::new(Token::String(String::from(*span.fragment())), &span),
    )(i)
}

#[tracable_parser]
fn literal_val(i: Span) -> Result {
    let (_, head): (_, char) = peek(anychar)(i)?;
    match head {
        'n' => null_literal(i),
        't' | 'f' => boolean(i),
        '"' => string(i),
        '<' => todo!(), // heredoc
        _ => Err(Err::Failure((i, ErrorKind::Char))),
    }
}

#[tracable_parser]
fn attribute(i: Span) -> Result {
    map(
        terminated(
            separated_pair(identifier, tuple((space0, char('='), space0)), literal_val),
            opt(newline),
        ),
        |(ident, value): (Node, Node)| {
            let ident = Rc::new(ident);
            let attr = Attribute {
                ident: Rc::clone(&ident),
                expr: Rc::new(value),
            };
            Node::from_node(Token::Attribute(attr), &ident)
        },
    )(i)
}

#[tracable_parser]
fn block_label(i: Span) -> Result {
    let (_, head): (_, char) = peek(anychar)(i)?;
    match head {
        '"' => string(i),
        ' ' => Err(Err::Error((i, ErrorKind::Char))),
        _ => identifier(i),
    }
}

#[tracable_parser]
fn block(i: Span) -> Result {
    map(
        tuple((
            identifier,
            preceded(
                space1,
                terminated(
                    separated_list(space1, block_label),
                    tuple((space0, char('{'), newline)),
                ),
            ),
            terminated(opt(body), tuple((space0, char('}'), newline))),
        )),
        |(ident, labels, body): (Node, Vec<Node>, Option<Node>)| {
            let ident = Rc::new(ident);
            let b = Block {
                ident: Rc::clone(&ident),
                body: body.map(Rc::new),
                labels,
            };
            Node::from_node(Token::Block(b), &ident)
        },
    )(i)
}

#[tracable_parser]
fn body(i: Span) -> Result {
    map(
        fold_many1(
            preceded(multispace0, alt((attribute, block))),
            Vec::new(),
            |mut body, node| {
                body.push(node);
                body
            },
        ),
        |body| Node::new(Token::Body(body), &i),
    )(i)
}

fn file(i: Span) -> OResult {
    let (_, tree) = complete(fold_many1(
        delimited(multispace0, alt((attribute, block)), multispace0),
        Tree::new(),
        |mut tree, node| {
            tree.push(node);
            tree
        },
    ))(i)?;

    Ok(tree)
}

pub fn parse_test(i: &str, info: TracableInfo) -> Result {
    let span = Span::new_extra(i, info);
    body(span)
}

pub fn parse_str(i: &str) -> OResult {
    let info = TracableInfo::default();
    let span = Span::new_extra(i, info);
    file(span)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_identifier() {
        let cases = vec![
            ("test", Token::Identifier("test".into())),
            (
                "test_with_underscores",
                Token::Identifier("test_with_underscores".into()),
            ),
            (
                "test-with-dashes",
                Token::Identifier("test-with-dashes".into()),
            ),
            (
                "test-14_with_numbers",
                Token::Identifier("test-14_with_numbers".into()),
            ),
        ];

        for (input, expected) in cases {
            let info = TracableInfo::new();
            let (span, actual) = identifier(Span::new_extra(input, info)).unwrap();
            assert_eq!(span.fragment().len(), 0);
            assert_eq!(actual.token, expected);
        }
    }

    #[test]
    fn test_boolean() {
        let info = TracableInfo::default();

        let cases = vec![("true", Token::True), ("false", Token::False)];

        for (input, expected) in cases {
            let input_span = Span::new_extra(input, info);
            let (span, node) = boolean(input_span).unwrap();
            assert_eq!(span.fragment().len(), 0);
            assert_eq!(node.token, expected);

            let parsed: bool = input.parse().unwrap();
            assert_eq!(node.token.as_boolean(), Some(parsed));
        }
    }

    #[test]
    fn test_string() {
        let info = TracableInfo::default();
        let input = Span::new_extra(r#""hello there""#, info);
        let (span, node) = string(input).unwrap();
        assert_eq!(span.fragment().len(), 0);
        assert_eq!(node.token.as_string(), Some("hello there"));
    }
}
