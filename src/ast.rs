use std::{error::Error, rc::Rc};

#[macro_use]
mod tokens;
mod collection;
mod literal;
mod node;
mod number;
mod operation;

use collection::collection;
use literal::{literal, single_line_string};
use number::number;
use operation::operation;

pub use node::Node;
pub use number::N;
pub use tokens::*;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while_m_n},
    character::complete::{anychar, char, multispace0, newline, space0, space1},
    combinator::{all_consuming, complete, map, opt, peek},
    error::ErrorKind,
    multi::{fold_many1, many0, separated_list},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
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
fn elipsis(i: Span) -> Result {
    map(tag("..."), |span| {
        Node::new(Token::Operator(Operator::Elipsis), &span)
    })(i)
}

#[tracable_parser]
fn function(i: Span) -> Result {
    map(
        pair(
            identifier,
            delimited(
                char('('),
                opt(tuple((
                    expr_term,
                    many0(preceded(pair(char(','), multispace0), expr_term)),
                    opt(elipsis),
                ))),
                char(')'),
            ),
        ),
        |(name, args)| {
            let name = Rc::new(name);
            let mut f = Function {
                name: Rc::clone(&name),
                args: Vec::new(),
            };
            if let Some((first, mut tail, maybe_elips)) = args {
                f.args.push(first);
                f.args.append(&mut tail);

                if let Some(elips) = maybe_elips {
                    let last = Rc::new(f.args.pop().unwrap());
                    let op = UnaryOp {
                        operator: Rc::new(elips),
                        operand: Rc::clone(&last),
                    };
                    let last = Node::from_node(Token::UnaryOp(op), &last);
                    f.args.push(last);
                }
            }

            Node::from_node(Token::Function(f), &name)
        },
    )(i)
}

#[tracable_parser]
fn expr_term(i: Span) -> Result {
    alt((literal, collection, function))(i)
}

#[tracable_parser]
fn conditional(i: Span) -> Result {
    todo!()
}

#[tracable_parser]
fn expression(i: Span) -> Result {
    //alt((expr_term, operation, conditional))(i)
    alt((expr_term, operation))(i)
}

#[tracable_parser]
fn attribute(i: Span) -> Result {
    map(
        terminated(
            separated_pair(identifier, tuple((space0, char('='), space0)), expr_term),
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
        single_line_string(i)
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
        .color(false)
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
    use rstest::{fixture, rstest};
    use std::convert::TryFrom;

    pub(super) type Result = std::result::Result<(), Box<dyn std::error::Error>>;

    #[fixture]
    pub(super) fn info() -> TracableInfo {
        TracableInfo::default()
    }

    #[rstest(input, expected,
            case("test", ident!("test")),
            case("test_with_underscores", ident!("test_with_underscores")),
            case("test-with-dashes", ident!("test-with-dashes")),
            case("test-14_with_numbers", ident!("test-14_with_numbers"))
    )]
    fn test_identfier(input: &'static str, expected: Token, info: TracableInfo) -> Result {
        let (span, actual) = identifier(Span::new_extra(input, info))?;
        assert_eq!(span.fragment().len(), 0);
        assert_eq!(actual.token, expected);

        Ok(())
    }

    #[rstest(input, expected,
            case("foo = null", attr!("foo", null!())),
            case("test_1 = true", attr!("test_1", boolean!(true))),
            case(
                r#"test-2 = "a test string""#,
                attr!("test-2", string!("a test string")),
            ),
            case(
                "another_test = -193.5\n",
                attr!("another_test", number!(-193.5)),
            )
    )]
    fn test_attribute(input: &'static str, expected: Token, info: TracableInfo) -> Result {
        let span = Span::new_extra(input, info);
        let (span, node) = attribute(span)?;
        assert_eq!(span.fragment().len(), 0);

        let attr = node
            .token
            .as_attribute()
            .ok_or("node.token was not an attribute")?;
        let expected = expected
            .as_attribute()
            .ok_or("expected was not an attribute")?;

        // compare just the tokens; the expected node location fields
        // are dummied and won't match
        assert_eq!(attr.ident.token, expected.ident.token,);
        attr.expr.assert_same_token(&expected.expr);

        Ok(())
    }

    #[rstest(input, expected,
        case("test_func(1, 2, 3)", function!("test_func", number!(1), number!(2), number!(3))),
        case("foo(false)", function!("foo", boolean!(false))),
        case(
            "bar([1, 2, 3]...)",
            function!("bar", unary!("...", list!(number!(1), number!(2), number!(3))))
        ),
    )]
    fn test_function(input: &'static str, expected: Token, info: TracableInfo) -> Result {
        let input = Span::new_extra(input, info);
        let (span, node) = function(input)?;
        assert_eq!(span.fragment().len(), 0);

        let f = node
            .token
            .as_function()
            .ok_or("node.token was not a function")?;
        let expected = expected
            .as_function()
            .ok_or("expected was not a function")?;
        f.name.assert_same_token(&expected.name);
        for (i, n) in expected.args.iter().enumerate() {
            f.args[i].assert_same_token(n);
        }

        Ok(())
    }
}
