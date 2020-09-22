use std::str::FromStr;

use super::TokenError;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum N {
    Int(i64),
    Float(f64),
}

impl Eq for N {}

impl From<i64> for N {
    fn from(i: i64) -> Self {
        N::Int(i)
    }
}

impl From<f64> for N {
    fn from(f: f64) -> Self {
        N::Float(f)
    }
}

impl N {
    pub fn is_int(&self) -> bool {
        self.as_int().is_some()
    }

    pub fn is_float(&self) -> bool {
        self.as_float().is_some()
    }

    pub fn as_int(&self) -> Option<i64> {
        if let N::Int(i) = self {
            Some(*i)
        } else {
            None
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        if let N::Float(f) = self {
            Some(*f)
        } else {
            None
        }
    }
}

impl FromStr for N {
    type Err = TokenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let i = s.parse::<i64>();
        if i.is_ok() {
            return Ok(N::Int(i?));
        }

        let f = s.parse::<f64>();
        if f.is_ok() {
            return Ok(N::Float(f?));
        }

        Err(f.err().unwrap().into())
    }
}
