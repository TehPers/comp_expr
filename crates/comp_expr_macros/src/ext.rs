use proc_macro2::Span;
use quote::ToTokens;
use std::fmt::Display;

pub trait OrError {
    type Tokens: ToTokens;

    fn or_error(self, error_span: Span, error_msg: impl Display) -> syn::Result<Self::Tokens>;
}

impl<'a, T: ToTokens> OrError for &'a Option<T> {
    type Tokens = &'a T;

    fn or_error(self, error_span: Span, error_msg: impl Display) -> syn::Result<Self::Tokens> {
        self.as_ref()
            .ok_or_else(|| syn::Error::new(error_span, error_msg))
    }
}
