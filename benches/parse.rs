use criterion::{black_box, criterion_group, criterion_main, Criterion};
use hcl2::ast::parse_str;

const TEST_BODY: &str = r#"
test block "label" {
    a = 1.4e9
    b = -73.19e-12

    internal {
        a = "test string here"
        b = 5
        c = 7.3
        d = -3
        e = -4.873
        k = <<EOF
        heredoc string
        is here, doc
        EOF
    }

    block two {
        a = false
        b = true
        c = null
        d = [1, 2, 3]
        e = [false, null, "string"]
    }
}

another test block {
    a = "another test string"
    b = 1.7e10
    c = false
}
"#;

pub fn benchmark(c: &mut Criterion) {
    c.bench_function("parse hcl2", |b| b.iter(|| black_box(parse_str(TEST_BODY))));
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
