#![allow(deprecated)]

use std::{
    io::{self, Read},
    time::Instant,
};

use hcl2::parser::parse_str;
use nom_tracable::{self, cumulative_histogram, histogram};

fn main() {
    let mut buf = vec![];
    let mut stdin = io::stdin();
    stdin.read_to_end(&mut buf).unwrap();

    let i = String::from_utf8(buf).unwrap();
    println!("input: {}", i);

    let now = Instant::now();
    let res = parse_str(&i).unwrap();
    let dur = now.elapsed();
    println!("parsed ast: {:#?}", res);

    if cfg!(feature = "trace") {
        histogram();
        cumulative_histogram();
    }

    println!("duration: {:?}", dur);
}
