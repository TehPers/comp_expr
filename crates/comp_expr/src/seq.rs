#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum SequenceState<T> {
    Yielded(T),
    Returned,
}

impl<T> From<T> for SequenceState<T> {
    fn from(value: T) -> Self {
        SequenceState::Yielded(value)
    }
}

pub fn bind<TSeq, TOut, ISeq, IOut, F>(
    seq: ISeq,
    mut f: F,
) -> impl Iterator<Item = SequenceState<TOut>>
where
    ISeq: IntoIterator<Item = TSeq>,
    IOut: IntoIterator<Item = SequenceState<TOut>>,
    F: FnMut(TSeq) -> IOut,
{
    seq.into_iter().flat_map(move |value| f(value))
}

pub fn delay<T, I, F>(f: F) -> F
where
    F: FnMut() -> I,
    I: IntoIterator<Item = SequenceState<T>>,
{
    f
}

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

pub fn r#loop<T, I, F>(body: F) -> impl Iterator<Item = T>
where
    I: IntoIterator<Item = T>,
    F: FnMut() -> I,
{
    std::iter::repeat_with(body).flatten()
}

pub fn r#yield<T>(item: T) -> impl Iterator<Item = SequenceState<T>> {
    std::iter::once(item.into())
}

pub fn r#return<T>(_: ()) -> impl Iterator<Item = SequenceState<T>> {
    std::iter::once(SequenceState::Returned)
}

pub fn zero<T>() -> impl Iterator<Item = T> {
    std::iter::empty()
}

pub fn run<T, I, F>(seq: F) -> impl Iterator<Item = T>
where
    I: IntoIterator<Item = SequenceState<T>>,
    F: FnOnce() -> I,
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
                    bind = $crate::seq::bind,
                    delay = $crate::seq::delay,
                    combine = $crate::seq::combine,
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
