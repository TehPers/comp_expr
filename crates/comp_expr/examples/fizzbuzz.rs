use comp_expr::seq;
use std::borrow::Cow;

fn main() {
    for n in fizzbuzz(100) {
        println!("{}", n);
    }
}

fn fizzbuzz(max: usize) -> impl IntoIterator<Item = Cow<'static, str>> {
    seq! {
        let? n = 1..=max;
        match (n % 3, n % 5) {
            (0, 0) => yield "FizzBuzz".into(),
            (0, _) => yield "Fizz".into(),
            (_, 0) => yield "Buzz".into(),
            _ => yield n.to_string().into(),
        }
    }
}
