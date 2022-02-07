use crate::{
    ext::OrError,
    types::{
        BindModifiers, Braced, CExprStmt, CExprStmts, DelayModifiers, DoBind, Else, ExecuteCExpr,
        For, FunctionDefs, If, LetBind, Loop, Match, MatchArm, MatchArms, Return, ReturnBind,
        While, Yield, YieldBind,
    },
};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{spanned::Spanned, Stmt};

pub trait Transform {
    fn is_cexpr(&self) -> bool;

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream>;
}

impl<T: Transform> Transform for Braced<T> {
    fn is_cexpr(&self) -> bool {
        self.inner.is_cexpr()
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        let inner = self.inner.transform(defs, then)?;
        let output = quote!({ #inner});
        Ok(output)
    }
}

impl Transform for Stmt {
    fn is_cexpr(&self) -> bool {
        false
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        let output = if !then.is_empty() {
            quote! {
                #self;
                #then
            }
        } else {
            // Get required definitions
            let zero = defs.zero.or_error(
                self.span(),
                "Missing 'zero' definition (needed for regular statement at end of block)",
            )?;

            // zero: impl Fn() -> M<T>
            quote! {
                #self;
                (#zero)()
            }
        };

        Ok(output)
    }
}

impl Transform for LetBind {
    fn is_cexpr(&self) -> bool {
        true
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        // Get required definitions
        let bind = defs.bind.or_error(
            self.target.span(),
            "Missing 'bind' definition (needed for 'let?')",
        )?;

        // let?: impl Fn(M<T>, impl FnOnce(T) -> M<U>) -> M<U>
        let value = self.value;
        let target = self.target;
        let ty = self.ty.as_ref().map(|(_, ty)| quote!(: #ty));
        let closure = if bind.modifiers.contains(BindModifiers::MOVE) {
            quote! { move |#target #ty| { #then } }
        } else {
            quote! { |#target #ty| { #then } }
        };
        let output = quote! {
            (#bind)(
                #value,
                #closure
            )
        };
        Ok(output)
    }
}

impl Transform for DoBind {
    fn is_cexpr(&self) -> bool {
        true
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        // Get required definitions
        let bind = defs.bind.or_error(
            self._do_token.span(),
            "Missing 'bind' definition (needed for 'do?')",
        )?;

        // do?: impl Fn(M<T>, impl FnOnce() -> M<U>) -> M<U>
        let value = self.value;
        let closure = if bind.modifiers.contains(BindModifiers::MOVE) {
            quote! { move |_| { #then } }
        } else {
            quote! { |_| { #then } }
        };
        let output = quote! {
            (#bind)(
                #value,
                #closure
            )
        };
        Ok(output)
    }
}

impl Transform for If {
    fn is_cexpr(&self) -> bool {
        self.then.is_cexpr() || self.r#else.as_ref().map(Transform::is_cexpr) == Some(true)
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        fn transform_then(defs: &FunctionDefs, inner: TokenStream) -> syn::Result<TokenStream> {
            match defs.then.as_ref() {
                Some(then) => {
                    let output = quote! {
                        (#then)(#inner)
                    };
                    Ok(output)
                },
                None => Ok(inner),
            }
        }

        fn transform_else(defs: &FunctionDefs, inner: TokenStream) -> syn::Result<TokenStream> {
            match defs.r#else.as_ref() {
                Some(r#else) => {
                    let output = quote! {
                        (#r#else)(#inner)
                    };
                    Ok(output)
                },
                None => Ok(inner),
            }
        }
        
        let condition = self.condition;
        let if_then = self.then.transform(defs, TokenStream::new())?;
        let if_then = transform_then(defs, if_then)?;
        let if_else = match self.r#else {
            None => {
                // Get required definitions
                let zero = defs.zero.or_error(
                    self.if_token.span(),
                    "Missing 'zero' definition (needed for regular statement at end of block)",
                )?;

                // zero: impl Fn() -> M<T>
                quote!((#zero)())
            }
            Some(if_else) => {
                if_else.transform(defs, TokenStream::new())?
            }
        };
        let if_else = transform_else(defs, if_else)?;
        let output = quote! {
            if (#condition) {
                #if_then
            } else {
                #if_else
            }
        };
        let output = if then.is_empty() {
            output
        } else {
            combine(defs, self.if_token.span(), output, then)?
        };

        Ok(output)
    }
}

impl Transform for Else {
    fn is_cexpr(&self) -> bool {
        match self {
            Else::ElseIf(_, inner) => inner.is_cexpr(),
            Else::Else(_, inner) => inner.is_cexpr(),
        }
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        let (else_token, output) = match self {
            Else::ElseIf(else_token, else_if) => {
                let else_if = else_if.transform(defs, TokenStream::new())?;
                let output = quote!(#r#else_if);
                (else_token, output)
            }
            Else::Else(else_token, else_block) => {
                let else_block = else_block.transform(defs, TokenStream::new())?;
                let output = quote!(#else_block);
                (else_token, output)
            }
        };
        let output = if then.is_empty() {
            output
        } else {
            combine(defs, else_token.span(), output, then)?
        };

        Ok(output)
    }
}

impl Transform for Match {
    fn is_cexpr(&self) -> bool {
        self.arms.is_cexpr()
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        let expr = self.expr;
        let arms = self.arms.transform(defs, TokenStream::new())?;
        let output = quote! {
            match #expr #arms
        };
        let output = if then.is_empty() {
            output
        } else {
            combine(defs, self.match_token.span(), output, then)?
        };

        Ok(output)
    }
}

impl Transform for MatchArms {
    fn is_cexpr(&self) -> bool {
        self.arms.iter().any(Transform::is_cexpr)
    }

    fn transform(self, defs: &FunctionDefs, _: TokenStream) -> syn::Result<TokenStream> {
        let arms: Vec<_> = self
            .arms
            .into_iter()
            .map(|arm| arm.transform(defs, TokenStream::new()))
            .collect::<Result<_, _>>()?;
        let output = quote! {
            #(#arms,)*
        };

        Ok(output)
    }
}

impl Transform for MatchArm {
    fn is_cexpr(&self) -> bool {
        self.body.is_cexpr()
    }

    fn transform(self, defs: &FunctionDefs, _: TokenStream) -> syn::Result<TokenStream> {
        let pat = self.pat;
        let guard = self.guard.map(|(_, guard)| quote!(if #guard));
        let body = self.body.transform(defs, TokenStream::new())?;
        let output = quote! {
            #guard #pat => { #body }
        };

        Ok(output)
    }
}

impl Transform for For {
    fn is_cexpr(&self) -> bool {
        self.body.is_cexpr()
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        // Get required definitions
        let r#for = defs
            .r#for
            .or_error(self.for_token.span(), "Missing 'for' definition")?;

        // for: impl Fn(Seq, impl Fn(TSeq) -> M<T>) -> M<T>
        let pat = self.pat;
        let expr = self.expr;
        let body = self.body.transform(defs, TokenStream::new())?;
        let output = quote! {
            (#r#for)(#expr, move |#pat| #body)
        };
        let output = if then.is_empty() {
            output
        } else {
            combine(defs, self.for_token.span(), output, then)?
        };
        Ok(output)
    }
}

impl Transform for While {
    fn is_cexpr(&self) -> bool {
        self.body.is_cexpr()
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        // Get required definitions
        let r#while = defs
            .r#while
            .or_error(self.while_token.span(), "Missing 'while' definition")?;

        // while: impl Fn(impl Fn() -> Cond, Delayed<T>) -> M<T>
        let cond = self.condition;
        let body = self.body.transform(defs, TokenStream::new())?;
        let body = delay(defs, body)?;
        let output = quote! {
            (#r#while)(
                move || { #cond },
                #body,
            )
        };
        let output = if then.is_empty() {
            output
        } else {
            combine(defs, self.while_token.span(), output, then)?
        };
        Ok(output)
    }
}

impl Transform for Loop {
    fn is_cexpr(&self) -> bool {
        self.body.is_cexpr()
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        // Get required definitions
        let r#loop = defs
            .r#loop
            .or_error(self.loop_token.span(), "Missing 'loop' definition")?;

        // loop: impl Fn(Delayed<T>) -> M<T>
        let body = self.body.transform(defs, TokenStream::new())?;
        let body = delay(defs, body)?;
        let output = quote! {
            (#r#loop)(#body)
        };
        let output = if then.is_empty() {
            output
        } else {
            combine(defs, self.loop_token.span(), output, then)?
        };
        Ok(output)
    }
}

impl Transform for Return {
    fn is_cexpr(&self) -> bool {
        true
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        // Get required definitions
        let r#return = defs
            .r#return
            .or_error(self.return_token.span(), "Missing 'return' definition")?;

        // return: impl Fn(T) -> M<T>
        let value = self.value.as_ref();
        let output = quote! { (#r#return)({ #value }) };
        let output = if then.is_empty() {
            output
        } else {
            combine(defs, self.return_token.span(), output, then)?
        };
        Ok(output)
    }
}

impl Transform for ReturnBind {
    fn is_cexpr(&self) -> bool {
        true
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        // Get required definitions
        let let_bind = defs.bind.or_error(
            self.return_token.span(),
            "Missing 'bind' definition (needed for 'return?')",
        )?;
        let r#return = defs.r#return.or_error(
            self.return_token.span(),
            "Missing 'return' definition (needed for 'return?')",
        )?;

        // return?: impl Fn(M<T>) -> M<T>
        let value = self.value.as_ref();
        let output = quote! {
            (#let_bind)(
                #value,
                move |x| {
                    (#r#return)(x)
                }
            )
        };
        let output = if then.is_empty() {
            output
        } else {
            combine(defs, self.return_token.span(), output, then)?
        };
        Ok(output)
    }
}

impl Transform for Yield {
    fn is_cexpr(&self) -> bool {
        true
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        // Get required definitions
        let r#yield = defs
            .r#yield
            .or_error(self.yield_token.span(), "Missing 'yield' definition")?;

        // yield: impl Fn(T) -> M<T>
        let value = self.value;
        let output = quote! { (#r#yield)({ #value }) };
        let output = if then.is_empty() {
            output
        } else {
            combine(defs, self.yield_token.span(), output, then)?
        };
        Ok(output)
    }
}

impl Transform for YieldBind {
    fn is_cexpr(&self) -> bool {
        true
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        // Get required definitions
        let let_bind = defs.bind.or_error(
            self.yield_token.span(),
            "Missing 'bind' definition (needed for 'yield?')",
        )?;
        let r#yield = defs.r#yield.or_error(
            self.yield_token.span(),
            "Missing 'yield' definition (needed for 'yield?')",
        )?;

        // yield: impl Fn(M<T>) -> M<T>
        let value = self.value;
        let output = quote! {
            (#let_bind)(
                #value,
                move |x| {
                    (#r#yield)(x)
                }
            )
        };
        let output = if then.is_empty() {
            output
        } else {
            combine(defs, self.yield_token.span(), output, then)?
        };
        Ok(output)
    }
}

impl Transform for CExprStmt {
    fn is_cexpr(&self) -> bool {
        todo!()
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        match self {
            CExprStmt::Normal(stmt) => stmt.transform(defs, then),
            CExprStmt::Block(stmts) => stmts.transform(defs, then),
            CExprStmt::LetBind(stmt) => stmt.transform(defs, then),
            CExprStmt::DoBind(stmt) => stmt.transform(defs, then),
            CExprStmt::If(stmt) => stmt.transform(defs, then),
            CExprStmt::Match(stmt) => stmt.transform(defs, then),
            CExprStmt::For(stmt) => stmt.transform(defs, then),
            CExprStmt::While(stmt) => stmt.transform(defs, then),
            CExprStmt::Loop(stmt) => stmt.transform(defs, then),
            CExprStmt::Return(stmt) => stmt.transform(defs, then),
            CExprStmt::ReturnBind(stmt) => stmt.transform(defs, then),
            CExprStmt::ReturnImplicit(value) => {
                // Get required definitions
                let r#return = defs
                    .r#return
                    .or_error(value.span(), "Missing 'return' definition")?;

                // return: impl Fn(T) -> M<T>
                let output = quote! { (#r#return)({ #value }) };
                let output = if then.is_empty() {
                    output
                } else {
                    combine(defs, value.span(), output, then)?
                };
                Ok(output)
            }
            CExprStmt::Yield(stmt) => stmt.transform(defs, then),
            CExprStmt::YieldBind(stmt) => stmt.transform(defs, then),
        }
    }
}

impl Transform for CExprStmts {
    fn is_cexpr(&self) -> bool {
        self.stmts.iter().any(|stmt| stmt.is_cexpr())
    }

    fn transform(self, defs: &FunctionDefs, then: TokenStream) -> syn::Result<TokenStream> {
        self.stmts
            .into_iter()
            .rev()
            .try_fold(then, |output, stmt| stmt.transform(defs, output))
    }
}

impl ExecuteCExpr {
    pub fn transform(self) -> syn::Result<TokenStream> {
        let output = self.expr.transform(&self.defs, TokenStream::new())?;

        // Delay if needed
        let output = delay(&self.defs, output)?;

        // Run if needed
        let output = if let Some(run) = self.defs.run.as_ref() {
            // run: impl Fn(Delayed<T>) -> M<T>
            quote! { (#run)({ #output }) }
        } else {
            // No run function
            output
        };

        Ok(output)
    }
}

fn delay(defs: &FunctionDefs, expr: TokenStream) -> syn::Result<TokenStream> {
    // Get optional definitions
    let delay = match defs.delay.as_ref() {
        Some(delay) => delay,
        None => {
            // delay: impl Fn(T) -> T
            return Ok(quote! {
                #expr
            });
        }
    };

    // delay: impl Fn(impl FnOnce() -> M<T>) -> Delayed<T>
    let inner = &delay.inner;
    if delay.modifiers.contains(DelayModifiers::MOVE) {
        Ok(quote! { (#inner)(move || { #expr }) })
    } else {
        Ok(quote! { (#inner)(|| { #expr }) })
    }
}

fn combine(
    defs: &FunctionDefs,
    error_span: Span,
    first_expr: TokenStream,
    second_expr: TokenStream,
) -> syn::Result<TokenStream> {
    // Get required definitions
    let combine = defs
        .combine
        .or_error(error_span, "Missing 'combine' definition")?;

    // combine: impl Fn(M<T>, Delayed<T>) -> M<T>
    let delayed = delay(defs, second_expr)?;
    Ok(quote! { (#combine)({ #first_expr }, { #delayed }) })
}
