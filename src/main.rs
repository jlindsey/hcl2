#![allow(deprecated)]

use std::io::{self, Read};

use hcl2::ast::parse_test;
use nom_tracable::{cumulative_histogram, histogram, TracableInfo};

#[cfg(not(feature = "trace"))]
fn tracable_info() -> TracableInfo {
    TracableInfo::default()
}

#[cfg(feature = "trace")]
fn tracable_info() -> TracableInfo {
    TracableInfo::new()
        .backward(true)
        .forward(true)
        .parser_width(60)
        .fold("term")
}

fn main() {
    let mut buf = vec![];
    let mut stdin = io::stdin();
    stdin.read_to_end(&mut buf).unwrap();

    let i = String::from_utf8(buf).unwrap();

    let res = parse_test(&i, tracable_info()).unwrap();
    println!("parsed ast: {:#?}", res.1);
    println!("remaining input: {:?}", res.0);

    if cfg!(feature = "trace") {
        histogram();
        cumulative_histogram();
    }
}
