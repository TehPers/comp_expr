use comp_expr::seq;
use std::borrow::Cow;

fn main() {
    for n in fizzbuzz(100) {
        println!("{}", n);
    }
}

fn fizzbuzz(max: usize) -> impl Iterator<Item = Cow<'static, str>> {
    seq! {
        // Bind the values from the range to n
        let? n = 1..=max;

        // Yield 'FizzBuzz', 'Fizz', 'Buzz', or n
        match (n % 3, n % 5) {
            (0, 0) => yield "FizzBuzz".into(),
            (0, _) => yield "Fizz".into(),
            (_, 0) => yield "Buzz".into(),
            _ => yield n.to_string().into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fizzbuzz_15_values() {
        let expected = vec![
            "1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", "11", "Fizz", "13",
            "14", "FizzBuzz",
        ];
        assert_eq!(fizzbuzz(15).collect::<Vec<_>>(), expected);
    }
}
