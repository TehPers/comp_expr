#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum SequenceState<T> {
    Yielded(T),
    Returned,
}

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Iterator for Either<L, R>
where
    L: Iterator,
    R: Iterator<Item = <L as Iterator>::Item>,
{
    type Item = <L as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Either::Left(l) => l.next(),
            Either::Right(r) => r.next(),
        }
    }
}

#[inline]
pub fn bind<'f, TSeq, TOut, ISeq, IOut, F>(
    seq: ISeq,
    mut f: F,
) -> impl 'f + Iterator<Item = SequenceState<TOut>>
where
    ISeq: 'f + IntoIterator<Item = TSeq>,
    IOut: 'f + IntoIterator<Item = SequenceState<TOut>>,
    F: 'f + FnMut(TSeq) -> IOut,
{
    seq.into_iter().flat_map(move |value| f(value))
}

#[inline]
pub fn delay<T>(x: T) -> T {
    x
}

#[inline]
pub fn combine<T, I1, I2, F>(first: I1, second: F) -> impl Iterator<Item = SequenceState<T>>
where
    I1: IntoIterator<Item = SequenceState<T>>,
    I2: IntoIterator<Item = SequenceState<T>>,
    F: FnOnce() -> I2,
{
    first
        .into_iter()
        .chain(std::iter::once_with(second).flatten())
}

#[inline]
pub fn if_then<L, R>(seq: L) -> Either<L, R> {
    Either::Left(seq)
}

#[inline]
pub fn if_else<L, R>(seq: R) -> Either<L, R> {
    Either::Right(seq)
}

#[inline]
pub fn r#for<TSeq, TBlock, ISeq, IBlock, FBlock>(
    items: ISeq,
    mut block: FBlock,
) -> impl Iterator<Item = TBlock>
where
    ISeq: IntoIterator<Item = TSeq>,
    IBlock: IntoIterator<Item = TBlock>,
    FBlock: FnMut(TSeq) -> IBlock,
{
    let mut items = items.into_iter();
    std::iter::from_fn(move || {
        let item = items.next()?;
        Some(block(item))
    })
    .flatten()
}

#[inline]
pub fn r#while<TSeq, IBody, FCond, FBody>(
    mut condition: FCond,
    mut body: FBody,
) -> impl Iterator<Item = TSeq>
where
    IBody: IntoIterator<Item = TSeq>,
    FCond: FnMut() -> bool,
    FBody: FnMut() -> IBody,
{
    std::iter::from_fn(move || if condition() { Some(body()) } else { None }).flatten()
}

#[inline]
pub fn r#loop<T, I, F>(body: F) -> impl Iterator<Item = T>
where
    I: IntoIterator<Item = T>,
    F: FnMut() -> I,
{
    std::iter::repeat_with(body).flatten()
}

#[inline]
pub fn r#yield<T>(item: T) -> impl Iterator<Item = SequenceState<T>> {
    std::iter::once(SequenceState::Yielded(item))
}

#[inline]
pub fn r#return<T>(_: ()) -> impl Iterator<Item = SequenceState<T>> {
    std::iter::once(SequenceState::Returned)
}

#[inline]
pub fn zero<T>() -> impl Iterator<Item = T> {
    std::iter::empty()
}

#[inline]
pub fn run<T, I, F>(seq: F) -> impl Iterator<Item = T>
where
    F: FnOnce() -> I,
    I: IntoIterator<Item = SequenceState<T>>,
{
    seq().into_iter().scan((), |_, state| match state {
        SequenceState::Yielded(value) => Some(value),
        SequenceState::Returned => None,
    })
}

#[macro_export]
macro_rules! seq {
    ($($tokens:tt)*) => {
        {
            $crate::c_expr! {
                defs = {
                    bind = [] $crate::seq::bind,
                    delay = [move] $crate::seq::delay,
                    combine = $crate::seq::combine,
                    then = $crate::seq::if_then,
                    else = $crate::seq::if_else,
                    for = $crate::seq::r#for,
                    while = $crate::seq::r#while,
                    loop = $crate::seq::r#loop,
                    yield = $crate::seq::r#yield,
                    return = $crate::seq::r#return,
                    zero = $crate::seq::zero,
                    run = $crate::seq::run,
                },
                expr = {
                    $($tokens)*
                }
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bind_works() {
        let seq = seq! {
            let? x = 1..=3;
            let? y = 4..=6;
            yield (x, y);
        };

        assert_eq!(
            seq.collect::<Vec<_>>(),
            vec![
                (1, 4),
                (1, 5),
                (1, 6),
                (2, 4),
                (2, 5),
                (2, 6),
                (3, 4),
                (3, 5),
                (3, 6),
            ]
        );
    }

    #[test]
    fn yield_works() {
        let seq = seq! {
            yield 1;
            yield 2;
            yield 3;
        };

        assert_eq!(seq.collect::<Vec<_>>(), vec![1, 2, 3]);
    }

    #[test]
    fn yield_bind_works() {
        let seq = seq! {
            yield? 1..=3;
        };

        assert_eq!(seq.collect::<Vec<_>>(), vec![1, 2, 3]);
    }

    #[test]
    fn return_works() {
        let seq = seq! {
            return;
            yield 1;
        };

        assert_eq!(seq.collect::<Vec<_>>(), vec![]);
    }

    #[test]
    fn delay_works() {
        let s = std::iter::from_fn(|| {
            unreachable!("This should never be called");
            #[allow(unreachable_code)]
            Some(1)
        });
        let seq = seq! {
            return;
            yield? s;
        };

        assert_eq!(seq.collect::<Vec<_>>(), vec![]);
    }

    #[test]
    fn if_works() {
        let seq = seq! {
            if true {
                yield 1;
            } else {
                yield 2;
            }
        };

        assert_eq!(seq.collect::<Vec<_>>(), vec![1]);
    }

    #[test]
    fn for_works() {
        let seq = seq! {
            for x in 1..=3 {
                yield x;
            }
        };

        assert_eq!(seq.collect::<Vec<_>>(), vec![1, 2, 3]);
    }

    #[test]
    fn while_works() {
        let seq = seq! {
            while true {
                yield 1;
            }
        };

        assert_eq!(seq.take(5).collect::<Vec<_>>(), vec![1, 1, 1, 1, 1]);
    }

    #[test]
    fn loop_works() {
        let seq = seq! {
            loop {
                yield 1;
            }
        };

        assert_eq!(seq.take(5).collect::<Vec<_>>(), vec![1, 1, 1, 1, 1]);
    }

    #[test]
    fn zero_works() {
        let seq = seq! {
            if false {
                yield 1;
            }
        };

        assert!(seq.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn fibonacci_works() {
        let seq = seq! {
            let mut x = 0;
            let mut y = 1;
            yield x;
            while true {
                let z = x + y;
                yield z;
                x = y;
                y = z;
            }
        };

        assert_eq!(
            seq.take(10).collect::<Vec<_>>(),
            vec![0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
        );
    }
}
