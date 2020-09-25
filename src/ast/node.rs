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

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::N;

    impl Node {
        pub(in crate::ast) fn assert_same_token(&self, other: &Node) {
            match &self.token {
                Token::Heredoc(doc) => {
                    if let Some(other_doc) = other.token.as_heredoc() {
                        assert_eq!(doc.truncate, other_doc.truncate);
                        assert_eq!(doc.content, other_doc.content);
                        doc.ident.assert_same_token(&other_doc.ident);
                    } else {
                        panic!(
                            "expected Heredoc, got {:#?}; self is {:#?}",
                            other.token, self.token
                        );
                    }
                }
                Token::BinaryOp(token) => {
                    if let Some(op) = other.token.as_binary_op() {
                        token.left.assert_same_token(&op.left);
                        token.right.assert_same_token(&op.right);
                        token.operator.assert_same_token(&op.operator);
                    } else {
                        panic!(
                            "expected BinaryOp, got {:#?}; self is {:#?}",
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
                            "expected UnaryOp, got {:#?}; self is {:#?}",
                            other.token, self.token
                        );
                    }
                }
                Token::Number(N::Float(f1)) => {
                    if let Some(N::Float(f2)) = other.token.as_number() {
                        assert!((f1 - f2).abs() < f64::EPSILON);
                    } else {
                        panic!(
                            "expected N, got {:#?}; self is {:#?}",
                            other.token, self.token
                        )
                    }
                }
                Token::List(items) | Token::Object(items) => {
                    let other_items = other
                        .token
                        .as_list()
                        .or_else(|| other.token.as_object())
                        .unwrap_or_else(|| {
                            panic!(
                                "expected list or object, got {:#?}; self is {:#?}",
                                other.token, self.token
                            )
                        });
                    for (i, item) in items.iter().enumerate() {
                        item.assert_same_token(&other_items[i]);
                    }
                }
                Token::ObjectItem(item) => {
                    if let Some(other_item) = other.token.as_object_item() {
                        item.key.assert_same_token(&other_item.key);
                        item.val.assert_same_token(&other_item.val);
                    } else {
                        panic!(
                            "expected object item, got {:#?}; self is {:#?}",
                            other.token, self.token
                        )
                    }
                }
                token => assert_eq!(token, &other.token),
            }
        }
    }
}
