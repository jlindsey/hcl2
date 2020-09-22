use std::{convert::TryFrom, error::Error, rc::Rc};

#[macro_use]
mod tokens;
mod node;
mod number;
pub use node::Node;
pub use number::N;
pub use tokens::*;

use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take_while, take_while_m_n},
    character::complete::{anychar, char, digit1, multispace0, newline, one_of, space0, space1},
    combinator::{all_consuming, complete, map, opt, peek, recognize},
    error::ErrorKind,
    multi::{fold_many1, many0, many_till, separated_list},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Err,
};
use nom_locate::LocatedSpan;
use nom_tracable::{tracable_parser, TracableInfo};

pub type Span<'a> = LocatedSpan<&'a str, TracableInfo>;

type SResult<O, E> = std::result::Result<O, E>;
type Result<'a, I = Span<'a>, O = Node, E = (I, ErrorKind)> = SResult<(I, O), nom::Err<E>>;
type OResult<'a> = SResult<Tree, Box<dyn Error + 'a>>;

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
fn single_line_string(i: Span) -> Result {
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
fn string(i: Span) -> Result {
    let (_, head): (_, char) = peek(anychar)(i)?;
    match head {
        '"' => single_line_string(i),
        '<' => heredoc(i),
        _ => Err(Err::Error((i, ErrorKind::Char))),
    }
}

#[tracable_parser]
fn sign(i: Span) -> Result {
    let (i, span) = alt((tag("-"), tag("+")))(i)?;
    Operator::try_from(*span.fragment())
        .map(|op| (i, Node::new(Token::Operator(op), &span)))
        .map_err(|_| Err::Error((span, ErrorKind::ParseTo)))
}

// TODO: factor this out to multiple parsers
// TODO: The negative sign is technically a uniary operation as defined by the spec
// and so should be factored out into the expression syntax parser once that is in
#[tracable_parser]
fn number(i: Span) -> Result {
    let start = i;
    let (i, maybe_sign) = opt(sign)(i)?;

    let (i, num) = map(
        tuple((digit1, opt(preceded(char('.'), digit1)))),
        |(dec, maybe_fract): (Span, Option<Span>)| {
            let mut buf = String::from(*dec.fragment());
            if let Some(fract) = maybe_fract {
                buf.push('.');
                buf.push_str(*fract.fragment());
            }

            let n: N = buf
                .parse()
                .map_err(|_| Err::Failure((dec, ErrorKind::ParseTo)))?;
            Ok(Node::new(Token::Number(n), &start))
        },
    )(i)?;

    if let Some(s) = maybe_sign {
        let s = Rc::new(s);
        let unary = UnaryOp {
            operator: Rc::clone(&s),
            operand: Rc::new(num?),
        };

        Ok((i, Node::from_node(Token::UnaryOp(unary), &s)))
    } else {
        Ok((i, num?))
    }
}

#[tracable_parser]
fn literal_val(i: Span) -> Result {
    let (_, head): (_, char) = peek(anychar)(i)?;
    match head {
        'n' => null_literal(i),
        't' | 'f' => boolean(i),
        '"' | '<' => string(i),
        '-' | '0'..='9' => number(i),
        _ => Err(Err::Error((i, ErrorKind::Char))),
    }
}

// NOTE: technically a "tuple" in the spec, but that name is reserved here
#[tracable_parser]
fn array(i: Span) -> Result {
    map(
        terminated(
            tuple((
                recognize(pair(char('['), multispace0)),
                opt(literal_val),
                many0(preceded(pair(char(','), multispace0), literal_val)),
            )),
            tuple((opt(char(',')), multispace0, char(']'))),
        ),
        |(span, first, mut tail): (Span, Option<Node>, Vec<Node>)| {
            let items = first.map_or(Vec::new(), |head| {
                let mut v: Vec<Node> = vec![head];
                v.append(&mut tail);
                v
            });
            Node::new(Token::List(items), &span)
        },
    )(i)
}

#[tracable_parser]
fn attribute(i: Span) -> Result {
    map(
        terminated(
            separated_pair(
                identifier,
                tuple((space0, char('='), space0)),
                alt((array, literal_val)),
            ),
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
    if head == '"' {
        string(i)
    } else {
        identifier(i)
    }
}

#[tracable_parser]
fn one_line_block(i: Span) -> Result {
    map(
        tuple((
            identifier,
            preceded(
                space1,
                terminated(
                    separated_list(space1, block_label),
                    tuple((space0, char('{'), space0)),
                ),
            ),
            terminated(opt(attribute), tuple((space0, char('}'), newline))),
        )),
        |(ident, labels, attr): (Node, Vec<Node>, Option<Node>)| {
            let ident = Rc::new(ident);
            let b = Block {
                ident: Rc::clone(&ident),
                body: attr
                    .map(|node| Rc::new(Node::from_node(Token::Body(vec![node.clone()]), &node))),
                labels,
            };
            Node::from_node(Token::Block(b), &ident)
        },
    )(i)
}

#[tracable_parser]
fn multi_line_block(i: Span) -> Result {
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
fn block(i: Span) -> Result {
    alt((one_line_block, multi_line_block))(i)
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
    let (_, tree) = all_consuming(complete(fold_many1(
        delimited(multispace0, alt((attribute, block)), multispace0),
        Tree::new(),
        |mut tree, node| {
            tree.push(node);
            tree
        },
    )))(i)?;

    Ok(tree)
}

#[cfg(feature = "trace")]
fn get_tracer() -> TracableInfo {
    TracableInfo::new()
        .backward(true)
        .forward(true)
        .parser_width(40)
        .fold("term")
}

#[cfg(not(feature = "trace"))]
fn get_tracer() -> TracableInfo {
    Default::default()
}

pub fn parse_str(i: &str) -> OResult {
    let span = Span::new_extra(i, get_tracer());
    file(span)
}

#[cfg(test)]
mod test {
    use super::*;

    impl Node {
        fn same_token(&self, other: &Node) -> bool {
            match &self.token {
                Token::BinaryOp(token) => other.token.as_binary_op().map_or(false, |op| {
                    token.left.same_token(&op.left)
                        && token.right.same_token(&op.right)
                        && token.operator.same_token(&op.operator)
                }),
                Token::UnaryOp(token) => other.token.as_unary_op().map_or(false, |op| {
                    token.operand.same_token(&op.operand) && token.operator.same_token(&op.operator)
                }),
                token => token == &other.token,
            }
        }
    }

    #[test]
    fn test_identifier() {
        let cases = vec![
            ("test", ident!("test")),
            ("test_with_underscores", ident!("test_with_underscores")),
            ("test-with-dashes", ident!("test-with-dashes")),
            ("test-14_with_numbers", ident!("test-14_with_numbers")),
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

        let cases = vec![("true", boolean!(true)), ("false", boolean!(false))];

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

        let cases = vec![
            (r#""hello there""#, string!("hello there")),
            (r#""with numbers 1 2 3""#, string!("with numbers 1 2 3")),
            (r#""escaped \"""#, string!("escaped \\\"")),
            (r#""escaped \n""#, string!("escaped \\n")),
        ];

        for (input, expected) in cases {
            let input = Span::new_extra(input, info);
            let (span, node) = string(input).unwrap();
            assert_eq!(span.fragment().len(), 0);
            assert_eq!(node.token, expected);
        }
    }

    #[test]
    fn test_number() {
        let info = TracableInfo::default();

        let cases = vec![
            ("1.23", number!(1.23)),
            ("47", number!(47)),
            ("17.3809", number!(17.3809)),
            ("17892037", number!(17892037)),
            ("-38", number!(-38)),
            ("-471.399", number!(-471.399)),
        ];

        for (input, expected) in cases {
            let span = Span::new_extra(input, info);
            let (span, node) = number(span).unwrap();
            assert_eq!(span.fragment().len(), 0);
            assert!(node.same_token(&node!(expected)));
        }
    }

    #[test]
    fn test_attribute() {
        let info = TracableInfo::default();

        let cases = vec![
            ("foo = null", attr!("foo", null!())),
            ("test_1 = true", attr!("test_1", boolean!(true))),
            (
                r#"test-2 = "a test string""#,
                attr!("test-2", string!("a test string")),
            ),
            (
                "another_test = -193.5\n",
                attr!("another_test", number!(-193.5)),
            ),
        ];

        for (input, expected) in cases {
            let span = Span::new_extra(input, info);
            let (span, node) = attribute(span).unwrap();
            assert_eq!(span.fragment().len(), 0);

            let attr = node.token.as_attribute().unwrap();
            let expected = expected.as_attribute().unwrap();

            // compare just the tokens; the expected node location fields
            // are dummied and won't match
            assert_eq!(attr.ident.token, expected.ident.token,);
            assert!(attr.expr.same_token(&expected.expr));
        }
    }

    #[test]
    fn test_tuple() {
        let info = TracableInfo::default();

        let cases = vec![
            (
                "[true, false, null]",
                list![boolean!(true), boolean!(false), null!()],
            ),
            (
                r#"[
                1,
                2,
                3,
               ]"#,
                list![number!(1), number!(2), number!(3)],
            ),
            ("[]", list![]),
            (
                r#"[
                    "test string",
                    "another string",
                    false,
                    17.38
                   ]"#,
                Token::List(vec![
                    node!(string!("test string")),
                    node!(string!("another string")),
                    node!(boolean!(false)),
                    node!(number!(17.38)),
                ]),
            ),
        ];

        for (input, expected) in cases {
            let span = Span::new_extra(input, info);
            let (span, node) = array(span).unwrap();
            assert_eq!(span.fragment().len(), 0);

            let expected = expected.as_list().unwrap();
            for (i, item) in node.token.as_list().unwrap().iter().enumerate() {
                assert_eq!(item.token, expected[i].token);
            }
        }
    }

    #[test]
    fn test_non_trunc_heredoc() {
        let info = TracableInfo::default();

        let test_str = "this is
        a test
        string
        in
        a
        heredoc\n";
        let input = format!("<<EOF\n{}EOF\n", test_str);
        let input = Span::new_extra(&input, info);
        let (span, node) = heredoc(input).unwrap();
        assert_eq!(span.fragment().len(), 0);

        let doc = node.token.as_heredoc().unwrap();
        assert_eq!(doc.ident.token, ident!("EOF"));
        assert!(!doc.truncate);
        assert_eq!(&doc.content, test_str);
    }

    #[test]
    fn test_trunc_heredoc() {
        let info = TracableInfo::default();

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
        assert_eq!(span.fragment().len(), 0);

        let doc = node.token.as_heredoc().unwrap();
        assert_eq!(doc.ident.token, ident!("EOF"));
        assert!(doc.truncate);
        assert_eq!(&doc.content, test_str);
    }
}
