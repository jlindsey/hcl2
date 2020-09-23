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
    bytes::complete::{escaped, is_not, tag, tag_no_case, take_while, take_while_m_n},
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

#[tracable_parser]
fn exponent(i: Span) -> Result<Span, i64> {
    let (i, (maybe_sign, num)) = preceded(tag_no_case("e"), pair(opt(sign), digit1))(i)?;

    let n: i64 = (*num.fragment())
        .parse()
        .map_err(|_| Err::Failure((i, ErrorKind::ParseTo)))?;

    if let Some(Node {
        token: Token::Operator(Operator::Minus),
        ..
    }) = maybe_sign
    {
        Ok((i, -n))
    } else {
        Ok((i, n))
    }
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

            Ok(n)
        },
    )(i)?;
    let num = num?;

    let (i, maybe_exp) = opt(exponent)(i)?;
    let num = maybe_exp.map_or(num, |exp| match num {
        N::Int(i) => {
            let pow = 10i64.pow(exp.abs() as u32);
            if exp < 0 {
                N::Int(i * (1 / pow))
            } else {
                N::Int(i * pow)
            }
        }
        N::Float(f) => {
            let pow = 10i64.pow(exp.abs() as u32) as f64;
            if exp < 0 {
                N::Float(f * (1.0 / pow))
            } else {
                N::Int((f * pow) as i64)
            }
        }
    });

    let num = Node::new(Token::Number(num), &start);

    if let Some(
        op @ Node {
            token: Token::Operator(Operator::Minus),
            ..
        },
    ) = maybe_sign
    {
        let op = Rc::new(op);
        let unary = UnaryOp {
            operator: Rc::clone(&op),
            operand: Rc::new(num),
        };

        Ok((i, Node::from_node(Token::UnaryOp(unary), &op)))
    } else {
        Ok((i, num))
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
                opt(expr_item),
                many0(preceded(pair(char(','), multispace0), expr_item)),
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
fn collection_val(i: Span) -> Result {
    array(i)
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
                    expr_item,
                    many0(preceded(pair(char(','), multispace0), expr_item)),
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
fn expr_item(i: Span) -> Result {
    alt((literal_val, collection_val, function))(i)
}

#[tracable_parser]
fn attribute(i: Span) -> Result {
    map(
        terminated(
            separated_pair(identifier, tuple((space0, char('='), space0)), expr_item),
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
    use rstest::rstest;

    type Result = std::result::Result<(), Box<dyn std::error::Error>>;
    #[rstest(input, expected,
            case("test", ident!("test")),
            case("test_with_underscores", ident!("test_with_underscores")),
            case("test-with-dashes", ident!("test-with-dashes")),
            case("test-14_with_numbers", ident!("test-14_with_numbers"))
    )]
    fn test_identfier(input: &'static str, expected: Token) -> Result {
        let info = TracableInfo::new();
        let (span, actual) = identifier(Span::new_extra(input, info))?;
        assert_eq!(span.fragment().len(), 0);
        assert_eq!(actual.token, expected);

        Ok(())
    }

    #[rstest(input, expected,
        case("true", boolean!(true)),
        case("false", boolean!(false))
    )]
    fn test_boolean(input: &'static str, expected: Token) -> Result {
        let info = TracableInfo::default();
        let i = Span::new_extra(input, info);
        let (span, node) = boolean(i)?;

        assert_eq!(span.fragment().len(), 0);
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
    fn test_string(input: &'static str, expected: Token) -> Result {
        let info = TracableInfo::default();
        let input = Span::new_extra(input, info);
        let (span, node) = string(input)?;
        assert_eq!(span.fragment().len(), 0);
        assert_eq!(node.token, expected);

        Ok(())
    }

    #[rstest(input, expected,
        case("1.23", number!(1.23)),
        case("47", number!(47)),
        case("17.3809", number!(17.3809)),
        case("17892037", number!(17892037)),
        case("-38", number!(-38)),
        case("-471.399", number!(-471.399)),
        case("1.7e8", number!(170000000)),
        case("-17E10", number!(-170000000000)),
        case("8.6e-6", number!(0.0000086))
    )]
    fn test_number(input: &'static str, expected: Token) -> Result {
        let info = TracableInfo::default();
        let span = Span::new_extra(input, info);
        let (span, node) = number(span)?;
        assert_eq!(span.fragment().len(), 0);
        node.assert_same_token(&node!(expected));

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
    fn test_attribute(input: &'static str, expected: Token) -> Result {
        let info = TracableInfo::default();
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
    fn test_tuple(input: &'static str, expected: Token) -> Result {
        let info = TracableInfo::default();
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
        case("test_func(1, 2, 3)", function!("test_func", number!(1), number!(2), number!(3))),
        case("foo(false)", function!("foo", boolean!(false))),
        case(
            "bar([1, 2, 3]...)",
            function!("bar", unary!("...", list!(number!(1), number!(2), number!(3))))
        ),
        ::trace
    )]
    fn test_function(input: &'static str, expected: Token) -> Result {
        let info = TracableInfo::default();
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

    impl Node {
        fn assert_same_token(&self, other: &Node) {
            match &self.token {
                Token::BinaryOp(token) => {
                    if let Some(op) = other.token.as_binary_op() {
                        token.left.assert_same_token(&op.left);
                        token.right.assert_same_token(&op.right);
                        token.operator.assert_same_token(&op.operator);
                    } else {
                        panic!(
                            "expected BinaryOp, got {:?}; self is {:?}",
                            other.token, self.token
                        );
                    }
                }
                Token::UnaryOp(token) => {
                    if let Some(op) = other.token.as_unary_op() {
                        token.operand.assert_same_token(&op.operand);
                        token.operator.assert_same_token(&op.operator);
                    } else {
                        panic!(
                            "expected UnaryOp, got {:?}; self is {:?}",
                            other.token, self.token
                        );
                    }
                }
                Token::Number(N::Float(f1)) => {
                    if let Some(N::Float(f2)) = other.token.as_number() {
                        assert!((f1 - f2).abs() < f64::EPSILON);
                    } else {
                        panic!(
                            "expected N, got {:?}; self is {:?}",
                            other.token, self.token
                        )
                    }
                }
                Token::List(list) => {
                    if let Some(other_list) = other.token.as_list() {
                        for (i, item) in list.iter().enumerate() {
                            item.assert_same_token(&other_list[i]);
                        }
                    } else {
                        panic!(
                            "expected list, got {:?}; self is {:?}",
                            other.token, self.token
                        )
                    }
                }
                token => assert_eq!(token, &other.token),
            }
        }
    }
}
