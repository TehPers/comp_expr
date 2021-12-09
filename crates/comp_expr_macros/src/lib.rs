extern crate proc_macro;

mod ext;
mod types;

#[proc_macro]
pub fn c_expr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: types::ExecuteCExpr = syn::parse_macro_input!(input);
    let output = match input.transform() {
        Ok(output) => output,
        Err(err) => err.to_compile_error(),
    };
    output.into()
}
