use std::rc::Rc;

use super::Span;
use paste::paste;

pub type Tree = Vec<Node>;

/// A single node in the AST containing a span and parsed token
#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub offset: usize,
    pub line: u32,
    pub column: u32,
    pub token: Token,
}

impl Default for Node {
    fn default() -> Self {
        Node {
            offset: 0,
            line: 0,
            column: 0,
            token: Token::Unknown,
        }
    }
}

impl Node {
    pub fn new(token: Token, span: &Span) -> Self {
        Node {
            token,
            offset: span.location_offset(),
            line: span.location_line(),
            column: span.get_utf8_column() as u32,
        }
    }

    pub fn from_node(token: Token, node: &Node) -> Self {
        Node {
            token,
            offset: node.offset,
            line: node.line,
            column: node.column,
        }
    }
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
    pub name: Node,
    pub args: Vec<Node>,
}

/// Attribute node
#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub ident: Rc<Node>,
    pub expr: Rc<Node>,
}

/// Block node
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub ident: Rc<Node>,
    pub labels: Vec<Node>,
    pub body: Option<Rc<Node>>,
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
    Int(i64),
    Float(f64),
    String(String),
    Heredoc(Heredoc),

    // Expression terms
    Function(Rc<Function>),

    // Containers
    List(Vec<Node>),
    Object(Vec<Node>),

    // Body elements
    LineComment(String),
    BlockComment(String),
    Attribute(Attribute),
    Block(Block),

    // Body
    Body(Vec<Node>),
}

impl Token {
    gen_as!(identifier, Token::Identifier(s), &str, s);

    gen_as!(null, Token::Null);
    gen_as!(true, Token::True);
    gen_as!(false, Token::False);
    gen_as!(int, Token::Int(i), &i64, i);
    gen_as!(float, Token::Float(i), &f64, i);
    gen_as!(string, Token::String(s), &str, s);
    gen_as!(heredoc, Token::Heredoc(h), &Heredoc, h);

    gen_as!(function, Token::Function(f), Rc<Function>, clone f);

    gen_as!(list, Token::List(l), &Vec<Node>, l);
    gen_as!(object, Token::Object(o), &Vec<Node>, o);

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
macro_rules! int {
    ($i:expr) => {
        Token::Int($i)
    };
}

#[macro_export]
macro_rules! float {
    ($f:expr) => {
        Token::Float($f)
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn as_null() {
        let a = Token::Null;
        assert_eq!(a.as_null(), Some(()));

        let b = Token::False;
        assert_eq!(b.as_null(), None);

        let c = Token::Identifier("testing".into());
        assert_eq!(c.as_null(), None);
    }

    #[test]
    fn as_identifier() {
        let a = Token::Identifier("test_ident".into());
        assert_eq!(a.as_identifier(), Some("test_ident"));
    }

    #[test]
    fn as_boolean() {
        let t = Token::True;
        assert_eq!(t.as_boolean(), Some(true));
        assert_eq!(t.as_true(), Some(()));

        let f = Token::False;
        assert_eq!(f.as_boolean(), Some(false));
        assert_eq!(f.as_false(), Some(()));
    }
}
