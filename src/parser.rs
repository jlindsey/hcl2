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
    bytes::complete::{tag, take, take_while, take_while_m_n},
    character::complete::{anychar, char, multispace0, newline, not_line_ending, space0, space1},
    combinator::{all_consuming, complete, map, opt, peek, recognize},
    error::ErrorKind,
    multi::{fold_many0, fold_many1, many0, separated_list},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Err,
};
use nom_locate::{position, LocatedSpan};
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
fn variable(i: Span) -> Result {
    identifier(i)
}

#[tracable_parser]
fn attr_access(i: Span) -> Result {
    let (i, span) = position(i)?;
    map(preceded(char('.'), identifier), move |node| {
        let unop = UnaryOp {
            operator: Rc::new(Node::new(Token::Operator(Operator::AttrAccess), &span)),
            operand: Rc::new(node),
        };
        Node::new(Token::UnaryOp(unop), &span)
    })(i)
}

#[tracable_parser]
fn index_access(i: Span) -> Result {
    let (i, span) = position(i)?;
    map(delimited(char('['), expression, char(']')), move |node| {
        let unop = UnaryOp {
            operator: Rc::new(Node::new(Token::Operator(Operator::IndexAccess), &span)),
            operand: Rc::new(node),
        };
        Node::new(Token::UnaryOp(unop), &span)
    })(i)
}

#[tracable_parser]
fn attr_splat(i: Span) -> Result {
    let (i, span) = position(i)?;
    let node = Node::new(Token::Operator(Operator::AttrSplat), &span);
    preceded(
        tag(".*"),
        fold_many0(attr_access, node, |node, attr| {
            let attr_op = attr.token.as_unary_op().cloned().unwrap();
            let node = Rc::new(node);
            let op = BinaryOp {
                left: Rc::clone(&node),
                operator: attr_op.operator,
                right: attr_op.operand,
            };

            Node::from_node(Token::BinaryOp(op), &node)
        }),
    )(i)
}

#[tracable_parser]
fn full_splat(i: Span) -> Result {
    let (i, span) = position(i)?;
    let node = Node::new(Token::Operator(Operator::FullSplat), &span);
    preceded(
        tag("[*]"),
        fold_many0(alt((attr_access, index_access)), node, |node, access| {
            let access_op = access.token.as_unary_op().cloned().unwrap();
            let node = Rc::new(node);
            let op = BinaryOp {
                left: Rc::clone(&node),
                operator: access_op.operator,
                right: access_op.operand,
            };

            Node::from_node(Token::BinaryOp(op), &node)
        }),
    )(i)
}

#[tracable_parser]
fn expr_postfix(i: Span) -> Result {
    let (_, head): (_, Span) = take(2usize)(i)?;
    let mut head = head.fragment().chars();
    match head.next() {
        Some('.') => {
            if let Some('*') = head.next() {
                attr_splat(i)
            } else {
                attr_access(i)
            }
        }
        Some('[') => {
            if let Some('*') = head.next() {
                full_splat(i)
            } else {
                index_access(i)
            }
        }
        _ => Err(Err::Error((i, ErrorKind::Alt))),
    }
}

#[tracable_parser]
fn sub_expression(i: Span) -> Result {
    delimited(
        tuple((char('('), multispace0)),
        expression,
        tuple((multispace0, char(')'))),
    )(i)
}

#[tracable_parser]
fn expr_term(i: Span) -> Result {
    let (rest, term) = alt((literal, collection, function, variable, sub_expression))(i)?;
    fold_many0(expr_postfix, term, |node, postfix| {
        let node = Rc::new(node);
        let postfix = Rc::new(postfix);

        let op = match &postfix.token {
            Token::BinaryOp(op) => BinaryOp {
                left: Rc::clone(&node),
                operator: op.operator.clone(),
                right: Rc::clone(&postfix),
            },
            Token::UnaryOp(op) => BinaryOp {
                left: Rc::clone(&node),
                operator: op.operator.clone(),
                right: op.operand.clone(),
            },
            _ => panic!("wrong type"),
        };
        Node::from_node(Token::BinaryOp(op), &node)
    })(rest)
}

#[tracable_parser]
fn conditional(i: Span) -> Result {
    let qm = recognize(tuple((space0, char('?'), space0)));
    let colon = recognize(tuple((space0, char(':'), space0)));
    map(
        tuple((expression, qm, expression, colon, expression)),
        |(cond, _, left, _, right)| {
            let cond = Rc::new(cond);
            let c = Conditional {
                condition: Rc::clone(&cond),
                if_true: Rc::new(left),
                if_false: Rc::new(right),
            };
            Node::from_node(Token::Conditional(c), &cond)
        },
    )(i)
}

#[tracable_parser]
fn expression(i: Span) -> Result {
    let (_, line): (_, Span) = peek(not_line_ending)(i)?;
    if line.fragment().contains(":?") {
        conditional(i)
    } else {
        alt((operation, expr_term))(i)
    }
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
    let (_, line): (_, Span) = peek(not_line_ending)(i)?;
    if line.fragment().contains('}') {
        one_line_block(i)
    } else {
        multi_line_block(i)
    }
}

#[tracable_parser]
fn body_item(i: Span) -> Result {
    let (_, line): (_, Span) = peek(not_line_ending)(i)?;
    if line.fragment().contains('=') {
        attribute(i)
    } else {
        block(i)
    }
}

#[tracable_parser]
fn body(i: Span) -> Result {
    map(
        fold_many1(
            preceded(multispace0, body_item),
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
        delimited(multispace0, body_item, multispace0),
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
            ),
            case(
                "testing = var.foo\n",
                attr!("testing", binary_op!(ident!("var"), ".", ident!("foo")))
            ),
            case(
                "testing = foo.*.bar.baz",
                attr!(
                    "testing",
                    binary_op!(
                        ident!("foo"),
                        ".",
                        binary_op!(
                            binary_op!(
                                Token::Operator(Operator::AttrSplat),
                                ".",
                                ident!("bar")
                            ),
                            ".",
                            ident!("baz")
                        )
                    )
                )
            ),
            case (
                "testing = foo[*].bar.baz",
                attr!(
                    "testing",
                    binary_op!(
                        ident!("foo"),
                        ".",
                        binary_op!(
                            binary_op!(
                                Token::Operator(Operator::FullSplat),
                                ".",
                                ident!("bar")
                            ),
                            ".",
                            ident!("baz")
                        )
                    )
                )
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
        assert_eq!(attr.ident.token, expected.ident.token);
        attr.expr.assert_same_token(&expected.expr);

        Ok(())
    }

    #[rstest(input, expected,
        case("test_func(1, 2, 3)", function!("test_func", number!(1), number!(2), number!(3))),
        case("foo(false)", function!("foo", boolean!(false))),
        case(
            "bar([1, 2, 3]...)",
            function!("bar", unary_op!("...", list!(number!(1), number!(2), number!(3))))
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

    #[rstest(input, expected,
        case(r#""string" ? true : false"#, conditional!(string!("string"), boolean!(true), boolean!(false))),
        case(
            r#"func("input") ? [1] : null"#,
            conditional!(function!("func", string!("input")), list!(number!(1)), null!())
        )
    )]
    fn test_conditional(input: &'static str, expected: Token, info: TracableInfo) -> Result {
        let input = Span::new_extra(input, info);
        let (span, node) = conditional(input)?;
        assert!(span.fragment().is_empty());

        let cond = node
            .token
            .as_conditional()
            .ok_or("node.token was not a conditional")?;
        let expected = expected
            .as_conditional()
            .ok_or("expected was not a conditional")?;

        cond.condition.assert_same_token(&expected.condition);
        cond.if_true.assert_same_token(&expected.if_true);
        cond.if_false.assert_same_token(&expected.if_false);

        Ok(())
    }
}
