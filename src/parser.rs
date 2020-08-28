#![allow(dead_code)]

use std::rc::Rc;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric1, char, digit1, multispace0, newline, space0, space1},
    combinator::{map, opt, value},
    error::{context, VerboseError},
    multi::{fold_many1, separated_list},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Element {
    Identifier(String),
    Function {
        name: Rc<Element>,
        args: Vec<Element>,
    },

    // Basic types
    Null,
    Int(i64),
    Float(f64),
    Boolean(bool),
    Str(String),

    // Containers
    List(Vec<Element>),
    //Object(HashMap<Element, Element>),

    // Body elements
    Attr {
        ident: Rc<Element>,
        value: Rc<Element>,
    },
    Block {
        ident: Rc<Element>,
        labels: Vec<Element>,
        body: Rc<Element>,
    },

    // Body
    Body {
        attrs: Vec<Element>,
        blocks: Vec<Element>,
    },
}

type Result<'a, I = &'a str, O = Element, E = VerboseError<I>> = IResult<I, O, E>;

fn identifier(i: &str) -> Result {
    context(
        "identifier",
        map(alphanumeric1, |s: &str| Element::Identifier(s.into())),
    )(i)
}

fn string_literal(i: &str) -> Result {
    map(delimited(char('"'), alphanumeric1, char('"')), |s: &str| {
        Element::Str(s.into())
    })(i)
}

fn true_literal(i: &str) -> Result<&str, bool, VerboseError<&str>> {
    value(true, tag("true"))(i)
}

fn false_literal(i: &str) -> Result<&str, bool, VerboseError<&str>> {
    value(false, tag("false"))(i)
}

fn boolean(i: &str) -> Result {
    context(
        "boolean",
        map(alt((true_literal, false_literal)), Element::Boolean),
    )(i)
}

fn null_literal(i: &str) -> Result {
    value(Element::Null, tag("null"))(i)
}

fn signed_multiplier(i: &str) -> Result<&str, i64, VerboseError<&str>> {
    map(opt(alt((char('-'), char('+')))), |maybe| match maybe {
        Some('-') => -1,
        _ => 1,
    })(i)
}

fn number_literal(i: &str) -> Result {
    map(tuple((signed_multiplier, digit1)), |(mult, digits)| {
        let val = digits.parse::<i64>().expect("unable to parse int");
        Element::Int(val * mult)
    })(i)
}

fn attribute(i: &str) -> Result {
    context(
        "attribute",
        map(
            preceded(
                space0,
                terminated(
                    separated_pair(
                        identifier,
                        tuple((space0, char('='), space0)),
                        alt((string_literal, boolean, number_literal, null_literal)),
                    ),
                    newline,
                ),
            ),
            move |(ident, value)| Element::Attr {
                ident: Rc::new(ident),
                value: Rc::new(value),
            },
        ),
    )(i)
}

fn block_label(i: &str) -> Result {
    alt((string_literal, identifier))(i)
}

fn block(i: &str) -> Result {
    context(
        "block",
        map(
            preceded(
                space0,
                tuple((
                    identifier,
                    preceded(
                        space1,
                        terminated(
                            separated_list(space1, block_label),
                            tuple((space0, char('{'), newline)),
                        ),
                    ),
                    terminated(body, tuple((space0, char('}'), newline))),
                )),
            ),
            move |(ident, labels, body)| Element::Block {
                ident: Rc::new(ident),
                body: Rc::new(body),
                labels,
            },
        ),
    )(i)
}

fn body(i: &str) -> Result {
    let (i, (blocks, attrs)) = context(
        "body",
        fold_many1(
            preceded(multispace0, alt((attribute, block))),
            (Vec::new(), Vec::new()),
            |mut vecs, elem| match elem {
                Element::Block { .. } => {
                    vecs.0.push(elem);
                    vecs
                }
                Element::Attr { .. } => {
                    vecs.1.push(elem);
                    vecs
                }
                _ => vecs,
            },
        ),
    )(i)?;

    Ok((i, Element::Body { blocks, attrs }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{error::convert_error, Err};

    macro_rules! make_attr {
        ($k:expr, $v:literal, str) => {
            Element::Attr {
                ident: Rc::new(make_ident!($k)),
                value: Rc::new(Element::Str(String::from($v))),
            }
        };
        ($k:expr, $v:literal, i64) => {
            Element::Attr {
                ident: Rc::new(make_ident!($k)),
                value: Rc::new(Element::Int($v)),
            }
        };
        ($k:expr, null) => {
            Element::Attr {
                ident: Rc::new(make_ident!($k)),
                value: Rc::new(Element::Null),
            }
        };
        ($k:expr, true) => {
            Element::Attr {
                ident: Rc::new(make_ident!($k)),
                value: Rc::new(Element::Boolean(true)),
            }
        };
        ($k:expr, false) => {
            Element::Attr {
                ident: Rc::new(make_ident!($k)),
                value: Rc::new(Element::Boolean(false)),
            }
        };
    }

    macro_rules! make_ident {
        ($k:literal) => {
            Element::Identifier(String::from($k))
        };
    }

    macro_rules! make_str {
        ($k:literal) => {
            Element::Str(String::from($k))
        };
    }

    macro_rules! test_matcher {
        ($input:ident, $f:ident, $rest:ident, $p:pat, $case:tt) => {
            match $f($input) {
                Err(Err::Failure(e)) => Err(format!("FAILURE: {}", convert_error($input, e))),
                Err(Err::Error(e)) => Err(format!("ERROR: {}", convert_error($input, e))),
                Err(e) => Err(format!("other err: {:#?}", e)),
                Ok(($rest, $p)) => {
                    $case
                    Ok(())
                }
                Ok((_, res)) => Err(format!("expected {:?}, got {:?}", stringify!($p), res)),
            }
        };
    }

    macro_rules! match_attrs {
        ($attrs:ident, $matchers:tt) => {
            let m = vec!$matchers;
            let mut i = 0;
            for elem in &$attrs {
                if let Element::Attr {
                    ref ident,
                    ref value,
                } = elem
                {
                    if let Element::Attr {
                        ident: expected_ident,
                        value: expected_value,
                    } = &m[i]
                    {
                        assert_eq!(**ident, **expected_ident);
                        assert_eq!(**value, **expected_value);
                    } else {
                        return Err(format!(
                            "expected item at match_attrs[{}] to be Element::Attr, got {:?}",
                            i, m[i]
                        ));
                    }

                    i += 1;
                } else {
                    return Err(format!(
                        "expected item at attrs[{}] to be Element::Attr, got {:?}",
                        i, elem
                    ));
                }
            }
        };
    }

    #[test]
    fn parse_attribute() -> std::result::Result<(), String> {
        let input = "foo = \"bar\"\n";
        test_matcher!(input, attribute, rest, Element::Attr { ident, value }, {
            assert_eq!(rest, "");
            assert_eq!(*ident, make_ident!("foo"));
            assert_eq!(*value, Element::Str("bar".to_string()));
        })
    }

    #[test]
    fn parse_simple_block() -> std::result::Result<(), String> {
        let input = "test block \"label\" {
            foo  = \"bar\"
            baz  = true
            quux = null
        }\n";
        test_matcher!(
            input,
            block,
            rest,
            Element::Block {
                ident,
                labels,
                body
            },
            {
                assert_eq!(rest, "");
                assert_eq!(*ident, make_ident!("test"));
                assert_eq!(labels.len(), 2);
                assert_eq!(labels[0], make_ident!("block"));
                assert_eq!(labels[1], make_str!("label"));

                let body = Rc::try_unwrap(body).unwrap();
                if let Element::Body { attrs, blocks } = body {
                    assert_eq!(
                        attrs.len(),
                        3,
                        "attrs.len() should be 3, was {}",
                        attrs.len()
                    );
                    assert_eq!(
                        blocks.len(),
                        0,
                        "blocks.len() should be 0, was {}",
                        blocks.len()
                    );

                    match_attrs!(
                        attrs,
                        [
                            make_attr!("foo", "bar", str),
                            make_attr!("baz", true),
                            make_attr!("quux", null),
                        ]
                    );
                } else {
                    return Err(format!("expected Element::Body, got {:?}", body));
                }
            }
        )
    }

    #[test]
    fn parse_nested_block() -> std::result::Result<(), String> {
        let input = "test block {
            internal {
                a = \"b\"
                c = 5
                d = false
                e = null
            }
        }\n";

        test_matcher!(
            input,
            block,
            rest,
            Element::Block {
                ident,
                labels,
                body
            },
            {
                assert_eq!(rest, "");
                assert_eq!(*ident, make_ident!("test"));
                assert_eq!(labels.len(), 1);
                assert_eq!(labels[0], make_ident!("block"));

                let body = Rc::try_unwrap(body).unwrap();
                if let Element::Body { attrs, mut blocks } = body {
                    assert_eq!(attrs.len(), 0);
                    assert_eq!(blocks.len(), 1);

                    let block = blocks.pop().unwrap();

                    if let Element::Block {
                        ident,
                        labels,
                        body,
                    } = block
                    {
                        assert_eq!(*ident, make_ident!("internal"));
                        assert_eq!(labels.len(), 0);

                        let body = Rc::try_unwrap(body).unwrap();
                        if let Element::Body { attrs, .. } = body {
                            assert_eq!(attrs.len(), 4);
                            match_attrs!(
                                attrs,
                                [
                                    make_attr!("a", "b", str),
                                    make_attr!("c", 5, i64),
                                    make_attr!("d", false),
                                    make_attr!("e", null),
                                ]
                            );
                        }
                    } else {
                        return Err(format!("expected Element::Block, got {:?}", blocks[0]));
                    }
                } else {
                    return Err(format!("expected Element::Body, got {:?}", body));
                }
            }
        )
    }
}
