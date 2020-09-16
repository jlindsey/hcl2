use criterion::{black_box, criterion_group, criterion_main, Criterion};
use hcl2::ast::parse_str;

const TEST_BODY: &str = "test block \"label\" {
    internal {
        a = \"test string here\"
        b = 5
        c = 7.3
        d = -3
        e = -4.873
        f = false
        g = true
        h = null
        i = [1, 2, 3]
        j = [false, null, \"string\"]
    }
}\n";

pub fn benchmark(c: &mut Criterion) {
    c.bench_function("parse hcl2", |b| b.iter(|| black_box(parse_str(TEST_BODY))));
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
