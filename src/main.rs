#![allow(deprecated)]

use std::io::{self, Read};

use hcl2::ast::parse_str;
use nom_tracable::{cumulative_histogram, histogram};

fn main() {
    let mut buf = vec![];
    let mut stdin = io::stdin();
    stdin.read_to_end(&mut buf).unwrap();

    let i = String::from_utf8(buf).unwrap();
    println!("input: {}", i);

    let res = parse_str(&i).unwrap();
    println!("parsed ast: {:#?}", res);

    if cfg!(feature = "trace") {
        histogram();
        cumulative_histogram();
    }
}
