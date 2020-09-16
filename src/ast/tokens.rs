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

    /// Fill in the location fields from a Span
    pub fn range_from_span(&mut self, span: &Span) {
        self.offset = span.location_offset();
        self.line = span.location_line();
        self.column = span.get_utf8_column() as u32;
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
    Identifier(String),

    // Basic literal types
    Null,
    True,
    False,
    Int(i64),
    Float(f64),
    String(String),

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
}
