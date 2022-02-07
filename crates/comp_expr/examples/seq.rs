use comp_expr::seq;
use std::{cell::RefCell, rc::Rc};

pub fn main() {
    let seq = seq! {
        println!("Begin sequence expression");

        // Yield multiple values at a time
        yield? [0, 1, 2, 3, 4];

        // Yield individual values in a for loop
        for n in 0..5 {
            yield n;
        }

        // Flatten an iterator into the current scope
        // The following code is executed on each value on the left side of the let? binding
        let? a = vec![0, 1, 2];
        let? b = vec![0, 1, 2];
        yield a + b; // Produces 0, 1, 2, 1, 2, 3, 2, 3, 4
    };

    println!("Iterating sequence");
    let values: Vec<_> = seq.collect();
    println!("Done");
    println!("Values: {:?}", values);

    // 'yield' and 'yield?' return values from the sequence expression
    let values: Vec<_> = seq! {
        yield 0; // 'yield' returns a single value
        yield? [1, 2, 3, 4]; // 'yield?' returns multiple values
    }
    .collect();
    assert_eq!(values, vec![0, 1, 2, 3, 4]);

    // 'let?' binds each value in a sequence to a variable, continuing execution for each one.
    let values: Vec<_> = seq! {
        let? a = [0, 1, 2];
        let? b = [0, 1, 2]; // This is executed 3 times
        yield a + b; // This is executed 9 times
    }
    .collect();
    assert_eq!(values, vec![0, 1, 2, 1, 2, 3, 2, 3, 4]);

    // Sequence expressions are lazily evaluated
    let s = seq! {
        println!("I won't be printed until the sequence is iterated!");
        yield 0;
        println!("I'll be printed after the first value is yielded");
    };
    println!("Iterating sequence");
    for n in s {
        println!("Got value: {}", n);
    }
    println!("Done iterating sequence");

    // Loops are also supported
    let values: Vec<_> = seq! {
        // For loops can bind a pattern to values from an iterator like normal
        for n in 0..5 {
            yield n;
        }

        // While loops can also be used, although mutable state is tricky within sequence expressions
        let n = Rc::new(RefCell::new(0));
        let condition = n.clone();
        while *condition.borrow() < 5 {
            let update = n.clone();
            yield *n.borrow();
            *update.borrow_mut() += 1;
        }

        // 'loop' can be used to execute an infinite loop
        loop {
            yield 0;
        }
    }
    .take(15)
    .collect();
    assert_eq!(values, vec![0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0]);
}
