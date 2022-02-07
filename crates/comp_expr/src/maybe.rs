// #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
// pub enum MaybeState<T> {
//     Return(T),
//     Some(T),
//     None,
// }

// pub fn bind<T, U, F>(value: Option<T>, f: F) -> Option<U>
// where
//     F: FnOnce(T) -> Option<U>,
// {
//     value.and_then(f)
// }

// pub fn delay<T, F>(f: F) -> F
// where
//     F: FnOnce() -> MaybeState<T>,
// {
//     f
// }

// pub fn combine<T, U, F>(prev: MaybeState<T>, f: F) -> MaybeState<U>
// where
//     F: FnOnce() -> MaybeState<U>,
// {
//     prev.and_then(move |_| f())
// }

// // pub fn r#for<E, S, F>(items: S, mut f: F) -> Option<()>
// // where
// //     S: IntoIterator<Item = E>,
// //     F: FnMut(E) -> Option<()>,
// // {
// //     items.into_iter().try_fold((), move |_, elem| f(elem))
// // }

// pub fn r#return<T>(value: T) -> MaybeState<T> {
//     MaybeState::Return(value)
// }

// pub fn zero() -> MaybeState<()> {
//     MaybeState::Some(())
// }

// pub fn run<T, F>(f: F) -> Option<T>
// where
//     F: FnOnce() -> Option<T>,
// {
//     f()
// }

// #[macro_export]
// macro_rules! maybe {
//     ($($tokens:tt)*) => {
//         {
//             $crate::c_expr! {
//                 defs = {
//                     bind = $crate::maybe::bind,
//                     delay = $crate::maybe::delay,
//                     combine = $crate::maybe::combine,
//                     for = $crate::maybe::r#for,
//                     while = $crate::maybe::r#while,
//                     loop = $crate::maybe::r#loop,
//                     return = $crate::maybe::r#return,
//                     zero = $crate::maybe::zero,
//                     run = $crate::maybe::run,
//                 },
//                 expr = {
//                     $($tokens)*
//                 }
//             }
//         }
//     };
// }
