use std::{
    convert::TryFrom,
    num::{ParseFloatError, ParseIntError},
    rc::Rc,
};

use super::{Node, N};
use paste::paste;
use thiserror::Error;

pub type Tree = Vec<Node>;

#[derive(Debug, Error)]
pub enum TokenError {
    #[error("unable to parse int: {0}")]
    ParseIntError(#[from] ParseIntError),

    #[error("unable to parse float: {0}")]
    ParseFloatError(#[from] ParseFloatError),

    #[error("unrecognized operator: {0}")]
    OperatorError(String),
}

/// Heredoc node
#[derive(Debug, Clone, PartialEq)]
pub struct Heredoc {
    pub ident: Rc<Node>,
    pub truncate: bool,
    pub content: String,
}

/// Function call node
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Rc<Node>,
    pub args: Vec<Node>,
}

/// Conditional (ternary operation) node
#[derive(Debug, Clone, PartialEq)]
pub struct Conditional {
    pub condition: Rc<Node>,
    pub if_true: Rc<Node>,
    pub if_false: Rc<Node>,
}

/// Uniary operation node
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOp {
    pub operator: Rc<Node>,
    pub operand: Rc<Node>,
}

/// Binary operation node
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOp {
    pub operator: Rc<Node>,
    pub left: Rc<Node>,
    pub right: Rc<Node>,
}

/// Attribute node
#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub ident: Rc<Node>,
    pub expr: Rc<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectItem {
    pub key: Rc<Node>,
    pub val: Rc<Node>,
}

/// Block node
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub ident: Rc<Node>,
    pub labels: Vec<Node>,
    pub body: Option<Rc<Node>>,
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Operator {
    // Arithmetic
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,

    // Logical
    And,
    Or,
    Not,

    // Comparison
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,

    // Postfix
    Elipsis,
    AttrAccess,
    IndexAccess,
    AttrSplat,
    FullSplat,
}

impl TryFrom<&str> for Operator {
    type Error = TokenError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "+" => Ok(Operator::Plus),
            "-" => Ok(Operator::Minus),
            "*" => Ok(Operator::Multiply),
            "/" => Ok(Operator::Divide),
            "%" => Ok(Operator::Modulus),

            "!" => Ok(Operator::Not),
            "&&" => Ok(Operator::And),
            "||" => Ok(Operator::Or),

            "==" => Ok(Operator::Equal),
            "!=" => Ok(Operator::NotEqual),
            ">" => Ok(Operator::Greater),
            "<" => Ok(Operator::Less),
            ">=" => Ok(Operator::GreaterEqual),
            "<=" => Ok(Operator::LessEqual),

            "..." => Ok(Operator::Elipsis),

            ".*" => Ok(Operator::AttrSplat),
            "[*]" => Ok(Operator::FullSplat),
            "[" => Ok(Operator::IndexAccess),
            "." => Ok(Operator::AttrAccess),

            _ => Err(TokenError::OperatorError(value.into())),
        }
    }
}

/// Tokens are the parsed elements of an HCL2 document
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// Unknown is used to be able to implement Default for Node, it should
    /// never actually be encountered outside this lib's internals and should
    /// be treated as a bug if it is.
    Unknown,

    Identifier(String),

    // Basic literal types
    Null,
    True,
    False,
    Number(N),
    String(String),
    Heredoc(Heredoc),

    // Expression terms
    Function(Function),
    Conditional(Conditional),
    Operator(Operator),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),

    // Containers
    List(Vec<Node>),
    Object(Vec<Node>),
    ObjectItem(ObjectItem),

    // Body elements
    LineComment(String),
    BlockComment(String),
    Attribute(Attribute),
    Block(Block),

    // Body
    Body(Vec<Node>),
}

macro_rules! gen_as {
    ($n:ident, $t:pat) => {
        paste! {
            pub fn [<as_ $n>](&self) -> Option<()> {
                if matches!(self, $t) {
                    Some(())
                } else {
                    None
                }
            }
        }
    };
    ($n:ident, $t:pat, $ot:ty, $o:tt) => {
        paste! {
            pub fn [<as_ $n>](&self) -> Option<$ot> {
                if let $t = self {
                    Some($o)
                } else {
                    None
                }
            }
        }
    };
    ($n:ident, $t:pat, $ot:ty, clone $o:tt) => {
        paste! {
            pub fn [<as_ $n>](&self) -> Option<$ot> {
                if let $t = self {
                    Some(Rc::clone($o))
                } else {
                    None
                }
            }
        }
    };
}

impl Token {
    gen_as!(identifier, Token::Identifier(s), &str, s);

    gen_as!(null, Token::Null);
    gen_as!(true, Token::True);
    gen_as!(false, Token::False);
    gen_as!(string, Token::String(s), &str, s);
    gen_as!(number, Token::Number(n), &N, n);
    gen_as!(heredoc, Token::Heredoc(h), &Heredoc, h);

    gen_as!(function, Token::Function(f), &Function, f);
    gen_as!(conditional, Token::Conditional(c), &Conditional, c);
    gen_as!(operator, Token::Operator(o), &Operator, o);
    gen_as!(binary_op, Token::BinaryOp(o), &BinaryOp, o);
    gen_as!(unary_op, Token::UnaryOp(u), &UnaryOp, u);

    gen_as!(list, Token::List(l), &Vec<Node>, l);
    gen_as!(object, Token::Object(o), &Vec<Node>, o);
    gen_as!(object_item, Token::ObjectItem(oi), &ObjectItem, oi);

    gen_as!(line_comment, Token::LineComment(s), &str, s);
    gen_as!(block_comment, Token::BlockComment(s), &str, s);
    gen_as!(attribute, Token::Attribute(a), &Attribute, a);
    gen_as!(block, Token::Block(b), &Block, b);
    gen_as!(body, Token::Body(b), &Vec<Node>, b);

    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            Token::True => Some(true),
            Token::False => Some(false),
            _ => None,
        }
    }
}

#[macro_export]
macro_rules! ident {
    ($s:expr) => {
        Token::Identifier(String::from($s))
    };
}

#[macro_export]
macro_rules! string {
    ($s:expr) => {
        Token::String(String::from($s))
    };
}

#[macro_export]
macro_rules! heredoc {
    ($i:expr, $t:expr, $s:expr) => {
        Token::Heredoc(Heredoc{
            ident: node!(rc ident!($i)),
            truncate: $t,
            content: String::from($s),
        })
    };
}

#[macro_export]
macro_rules! operator {
    ($s:expr) => {
        Token::Operator(Operator::try_from($s).unwrap())
    };
}

#[macro_export]
macro_rules! function {
    ($n:expr, $($a:expr),*) => {
        Token::Function(Function{
            name: node!(rc ident!($n)),
            args: vec![$(node!($a),)*],
        })
    };
}

#[macro_export]
macro_rules! number {
    (-$s:expr) => {
        Token::UnaryOp(UnaryOp{
            operator: node!(rc operator!("-")),
            operand: node!(rc number!($s)),
        })
    };
    ($s:expr) => {
        Token::Number($s.into())
    };
}

#[macro_export]
macro_rules! boolean {
    (true) => {
        Token::True
    };
    (false) => {
        Token::False
    };
}

#[macro_export]
macro_rules! null {
    () => {
        Token::Null
    };
}

#[macro_export]
macro_rules! node {
    (rc $t:expr) => {
        Rc::new(node!($t))
    };

    ($t:expr) => {
        Node {
            token: $t,
            ..Default::default()
        }
    };
}

#[macro_export]
macro_rules! attr {
    ($i:expr, $e:expr) => {
        Token::Attribute(Attribute {
            ident: node!(rc ident!($i)),
            expr: node!(rc $e),
        })
    };
}

#[macro_export]
macro_rules! list {
    ($($i:expr),*) => {
        Token::List(vec![$(node!($i),)*])
    }
}

#[macro_export]
macro_rules! object_item {
    ($k:expr => $v:expr) => {
        Token::ObjectItem(ObjectItem{key: node!(rc $k), val: node!(rc $v)})
    }
}

#[macro_export]
macro_rules! object {
    ($($k:expr => $v:expr),*) => {
        Token::Object(vec![$(node!(object_item!($k => $v)),)*])
    }
}

#[macro_export]
macro_rules! unary_op {
    ($o:expr, $i:expr) => {
        Token::UnaryOp(UnaryOp {
            operator: node!(rc operator!($o)),
            operand: node!(rc $i),
        })
    }
}

#[macro_export]
macro_rules! binary_op {
    ($l:expr, $o:expr, $r:expr) => {
        Token::BinaryOp(BinaryOp{
            left: node!(rc $l),
            operator: node!(rc operator!($o)),
            right: node!(rc $r),
        })
    }
}

#[macro_export]
macro_rules! conditional {
    ($t:expr, $l:expr, $r:expr) => {
        Token::Conditional(Conditional{
            condition: node!(rc $t),
            if_true: node!(rc $l),
            if_false: node!(rc $r),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn as_null() {
        assert_eq!(null!().as_null(), Some(()));
        assert_eq!(boolean!(false).as_null(), None);
        assert_eq!(ident!("testing").as_null(), None);
    }

    #[test]
    fn as_identifier() {
        assert_eq!(ident!("test_ident").as_identifier(), Some("test_ident"));
    }

    #[test]
    fn as_boolean() {
        let t = boolean!(true);
        assert_eq!(t.as_boolean(), Some(true));
        assert_eq!(t.as_true(), Some(()));

        let f = boolean!(false);
        assert_eq!(f.as_boolean(), Some(false));
        assert_eq!(f.as_false(), Some(()));
    }
}
