use std::rc::Rc;

use super::{expression, identifier, Node, ObjectItem, Result, Span, Token};

use nom::{
    branch::alt,
    character::complete::{anychar, char, multispace0},
    combinator::{map, opt, peek, recognize},
    error::ErrorKind,
    multi::many0,
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    Err,
};
use nom_tracable::tracable_parser;

// NOTE: technically a "tuple" in the spec, but that name is reserved here
#[tracable_parser]
fn array(i: Span) -> Result {
    map(
        terminated(
            tuple((
                recognize(pair(char('['), multispace0)),
                opt(expression),
                many0(preceded(pair(char(','), multispace0), expression)),
            )),
            tuple((opt(char(',')), multispace0, char(']'))),
        ),
        |(start, first, mut tail): (Span, Option<Node>, Vec<Node>)| {
            let items = first.map_or(Vec::new(), |head| {
                let mut v = vec![head];
                v.append(&mut tail);
                v
            });
            Node::new(Token::List(items), &start)
        },
    )(i)
}

#[tracable_parser]
fn object_item(i: Span) -> Result {
    map(
        separated_pair(
            alt((identifier, expression)),
            tuple((multispace0, alt((char(':'), char('='))), multispace0)),
            expression,
        ),
        |(key, val)| {
            let key = Rc::new(key);
            let val = Rc::new(val);
            Node::from_node(
                Token::ObjectItem(ObjectItem {
                    key: Rc::clone(&key),
                    val,
                }),
                &key,
            )
        },
    )(i)
}

#[tracable_parser]
fn object(i: Span) -> Result {
    map(
        terminated(
            tuple((
                recognize(pair(char('{'), multispace0)),
                opt(object_item),
                many0(preceded(pair(char(','), multispace0), object_item)),
            )),
            tuple((opt(char(',')), multispace0, char('}'))),
        ),
        |(start, first, mut tail)| {
            let items = first.map_or(Vec::new(), |head| {
                let mut v = vec![head];
                v.append(&mut tail);
                v
            });
            Node::new(Token::Object(items), &start)
        },
    )(i)
}

#[tracable_parser]
pub(super) fn collection(i: Span) -> Result {
    let (_, head): (_, char) = peek(anychar)(i)?;
    match head {
        '[' => array(i),
        '{' => object(i),
        _ => Err(Err::Error((i, ErrorKind::Char))),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::test::{info, Result};

    use nom_tracable::TracableInfo;
    use rstest::rstest;

    #[rstest(input, expected,
        case(
            "[true, false, null]",
            list![boolean!(true), boolean!(false), null!()],
        ),
       case(
            r#"[
            1,
            2,
            3,
           ]"#,
            list![number!(1), number!(2), number!(3)],
        ),
       case("[]", list![]),
       case(
           r#"[1, ["nested list", null]]"#,
           list![number!(1), list![string!("nested list"), null!()]]),
       case(
            r#"[
                "test string",
                "another string",
                false,
                17.38
               ]"#,
            list![
                string!("test string"),
                string!("another string"),
                boolean!(false),
                number!(17.38)
            ],
        ),
    )]
    fn test_tuple(input: &'static str, expected: Token, info: TracableInfo) -> Result {
        let span = Span::new_extra(input, info);
        let (span, node) = array(span)?;
        assert_eq!(span.fragment().len(), 0);

        let expected = expected.as_list().ok_or("expected was not a list")?;
        let items = node.token.as_list().ok_or("node.token was not a list")?;
        for (i, item) in items.iter().enumerate() {
            item.assert_same_token(&expected[i])
        }

        Ok(())
    }

    #[rstest(input, expected,
    case(r#"{test: "string"}"#, object!(ident!("test") => string!("string"))),
    case(r#"{test = "string"}"#, object!(ident!("test") => string!("string"))),
    case(
        "{test = 1, test_2 = 2.2, test_3: true}",
        object!(
            ident!("test") => number!(1),
            ident!("test_2") => number!(2.2),
            ident!("test_3") => boolean!(true)
        )),
    case(
        "{complex = {nested = {object = [1, 2, 3]}}}",
        object!(
            ident!("complex") => object!(
                ident!("nested") => object!(
                    ident!("object") => list!(number!(1), number!(2), number!(3))
                )
            )
        )
    )
    )]
    fn test_object(input: &'static str, expected: Token, info: TracableInfo) -> Result {
        let span = Span::new_extra(input, info);
        let (span, node) = object(span)?;
        assert!(span.fragment().is_empty());

        let expected = expected.as_object().ok_or("expected was not an object")?;
        let items = node
            .token
            .as_object()
            .ok_or("node.token was not an object")?;
        for (i, item) in items.iter().enumerate() {
            item.assert_same_token(&expected[i])
        }

        Ok(())
    }
}
