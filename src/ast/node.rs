use std::fmt::Debug;

use super::{Span, Token};

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
