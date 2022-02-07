use crate::types::{
    BindModifiers, CombineModifiers, DelayModifiers, ElseModifiers, ForModifiers, LoopModifiers,
    ReturnModifiers, RunModifiers, ThenModifiers, WhileModifiers, YieldModifiers, ZeroModifiers, Braced, CExprStmt, CExprStmts, DoBind, Else, ExecuteCExpr, For, FunctionDefs, If, LetBind, Loop, Match, MatchArm, MatchArms, Modified, Return, ReturnBind, While, Yield, YieldBind,
};
use proc_macro2::Span;
use syn::{
    braced, bracketed,
    ext::IdentExt,
    parse::{Parse, ParseStream},
    token::{Brace, Bracket},
    Expr, Ident, Lifetime, Stmt, Token,
};

macro_rules! parse_kvs {
    (@key_name $field_name:ident $field_key:tt?) => {
        concat!(stringify!($field_key), "?")
    };
    (@key_name $field_name:ident $field_key:tt) => {
        stringify!($field_key)
    };
    (@key_name $field_name:ident) => {
        stringify!($field_name)
    };
    (
        $input:expr,
        required = {
            $($([$($req_key:tt)*])? $req_name:ident: $req_ty:ty),*
            $(,)?
        },
        optional = {
            $($([$($opt_key:tt)*])? $opt_name:ident: $opt_ty:ty),*
            $(,)?
        }
    ) => {
        // Required fields
        $(let mut $req_name: Option<$req_ty> = None;)*
        $(let mut $opt_name: Option<$opt_ty> = None;)*

        // Parse input
        let input = $input;
        while input.peek(Ident::peek_any) {
            let key = input.call(Ident::parse_any)?;
            let mut key_name = key.to_string();
            if input.peek(Token![?])             {
                let _: Token![?] = input.parse()?;
                key_name.push('?');
            }
            let _: Token![=] = input.parse()?;

            match key_name.as_str() {
                $(parse_kvs!(@key_name $req_name $($($req_key)*)?) => {
                    if $req_name.is_some() {
                        return Err(syn::Error::new(
                            key.span(),
                            concat!("Duplicate field: '", parse_kvs!(@key_name $req_name $($($req_key)*)?), "'")
                        ));
                    }
                    $req_name = Some(input.parse()?);
                })*
                $(parse_kvs!(@key_name $opt_name $($($opt_key)*)?) => {
                    if $opt_name.is_some() {
                        return Err(syn::Error::new(
                            key.span(),
                            concat!("Duplicate field: '", parse_kvs!(@key_name $opt_name $($($opt_key)*)?), "'")
                        ));
                    }
                    $opt_name = Some(input.parse()?);
                })*
                _ => {
                    let msg = format!(
                        concat!(
                            "Expected ",
                            concat!(
                                "one of ",
                                $(concat!("'", parse_kvs!(@key_name $req_name $($($req_key)*)?), "', "),)*
                                $(concat!("'", parse_kvs!(@key_name $opt_name $($($opt_key)*)?), "', "),)*
                            ),
                            "found '{}'",
                        ),
                        key
                    );
                    return Err(syn::Error::new(key.span(), msg));
                }
            }

            if input.peek(Token![,]) {
                let _: Token![,] = input.parse()?;
            } else {
                break;
            }
        }

        $(
            let $req_name = $req_name
                .ok_or_else(|| {
                    syn::Error::new(
                        Span::call_site(),
                        concat!(
                            "Missing '",
                            parse_kvs!(@key_name $req_name $($($req_key)*)?),
                            "'"
                        )
                    )
                })?;
        )*
    };
}

macro_rules! parse_modifiers {
    ($name:ty = [$(($mod_name:ident, $token:tt)),* $(,)?]) => {
        impl Parse for $name {
            #[allow(unreachable_code)]
            fn parse(input: ParseStream) -> syn::Result<Self> {
                #[allow(unused_mut)]
                let mut modifiers = Self::empty();
                while !input.is_empty() {
                    let modifier = input.call(Ident::parse_any)?;
                    #[allow(unused_variables)]
                    let modifier = match modifier.to_string().as_str() {
                        $(stringify!($token) => Self::$mod_name,)*
                        _ => {
                            let allowed_modifiers: Vec<&'static str> = vec![
                                $(concat!("'", stringify!($token), "'"),)*
                            ];
                            let allowed_modifiers = if allowed_modifiers.is_empty() {
                                format!("no modifiers allowed")
                            } else {
                                let allowed_modifiers = allowed_modifiers.join(", ");
                                format!("allowed modifiers: {allowed_modifiers}")
                            };
                            return Err(syn::Error::new(
                                modifier.span(),
                                format!("Unknown modifier: '{modifier}' ({allowed_modifiers})"),
                            ))
                        }
                    };
                    modifiers |= modifier;

                    // Check for comma
                    if input.peek(Token![,]) {
                        let _: Token![,] = input.parse()?;
                    } else if !input.is_empty() {
                        return Err(input.error("Expected comma."));
                    }
                }

                Ok(modifiers)
            }
        }
    };
}

impl<T: Parse> Parse for Braced<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let input = {
            let content;
            braced!(content in input);
            content
        };
        Ok(Braced {
            inner: input.parse()?,
        })
    }
}

impl Parse for FunctionDefs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_kvs! {
            input,
            required = {},
            optional = {
                bind: Modified<BindModifiers, Expr>,
                delay: Modified<DelayModifiers, Expr>,
                combine: Modified<CombineModifiers,Expr>,
                then: Modified<ThenModifiers, Expr>,
                [else] r#else: Modified<ElseModifiers, Expr>,
                [for] r#for: Modified<ForModifiers, Expr>,
                [while] r#while: Modified<WhileModifiers, Expr>,
                [loop] r#loop: Modified<LoopModifiers, Expr>,
                [return] r#return: Modified<ReturnModifiers, Expr>,
                [yield] r#yield: Modified<YieldModifiers, Expr>,
                zero: Modified<ZeroModifiers, Expr>,
                run: Modified<RunModifiers, Expr>,
            }
        }

        Ok(FunctionDefs {
            bind,
            delay,
            combine,
            then,
            r#else,
            r#for,
            r#while,
            r#loop,
            r#return,
            r#yield,
            zero,
            run,
        })
    }
}

impl<M: Default + Parse, T: Parse> Parse for Modified<M, T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let (modifiers, modifiers_span) = if input.peek(Bracket) {
            let content;
            bracketed!(content in input);
            let span = content.span();
            (content.parse()?, Some(span))
        } else {
            (Default::default(), None)
        };
        let inner = input.parse()?;
        Ok(Modified {
            modifiers,
            modifiers_span,
            inner,
        })
    }
}

parse_modifiers!(BindModifiers = [(MOVE, move)]);
parse_modifiers!(DelayModifiers = [(MOVE, move)]);
parse_modifiers!(CombineModifiers = []);
parse_modifiers!(ThenModifiers = []);
parse_modifiers!(ElseModifiers = []);
parse_modifiers!(ForModifiers = []);
parse_modifiers!(WhileModifiers = []);
parse_modifiers!(LoopModifiers = []);
parse_modifiers!(ReturnModifiers = []);
parse_modifiers!(YieldModifiers = []);
parse_modifiers!(ZeroModifiers = []);
parse_modifiers!(RunModifiers = []);

impl Parse for LetBind {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let let_token: Token![let] = input.parse()?;
        let question_token: Token![?] = input.parse()?;
        let target = input.parse()?;
        let ty = if input.peek(Token![:]) {
            Some((input.parse()?, input.parse()?))
        } else {
            None
        };
        let eq_token: Token![=] = input.parse()?;
        let value = input.parse()?;
        Ok(LetBind {
            _let_token: let_token,
            _question_token: question_token,
            target,
            ty,
            _eq_token: eq_token,
            value,
        })
    }
}

impl Parse for DoBind {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let do_token: Token![do] = input.parse()?;
        let question_token: Token![?] = input.parse()?;
        let value = input.parse()?;
        Ok(DoBind {
            _do_token: do_token,
            _question_token: question_token,
            value,
        })
    }
}

impl Parse for If {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let if_token: Token![if] = input.parse()?;
        let condition = input.call(Expr::parse_without_eager_brace)?;
        let then = input.parse()?;
        let r#else = if input.peek(Token![else]) {
            Some(input.parse()?)
        } else {
            None
        };
        Ok(If {
            if_token,
            condition,
            then,
            r#else,
        })
    }
}

impl Parse for Else {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let else_token = input.parse()?;
        if input.peek(Token![if]) {
            let next = input.parse()?;
            Ok(Else::ElseIf(else_token, next))
        } else {
            let next = input.parse()?;
            Ok(Else::Else(else_token, next))
        }
    }
}

impl Parse for Match {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let match_token: Token![match] = input.parse()?;
        let expr = input.call(Expr::parse_without_eager_brace)?;
        let arms = input.parse()?;
        Ok(Match {
            match_token,
            expr,
            arms,
        })
    }
}

impl Parse for MatchArms {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut arms = Vec::new();
        while !input.is_empty() {
            arms.push(input.parse()?);
        }

        Ok(MatchArms { arms })
    }
}

impl Parse for MatchArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let pat = input.parse()?;
        let guard = if input.peek(Token![if]) {
            let if_token: Token![if] = input.parse()?;
            let guard = input.parse()?;
            Some((if_token, guard))
        } else {
            None
        };
        let fat_arrow_token: Token![=>] = input.parse()?;
        let body = input.parse()?;
        let comma = input.parse()?;
        Ok(MatchArm {
            pat,
            guard,
            _fat_arrow_token: fat_arrow_token,
            body,
            _comma: comma,
        })
    }
}

impl Parse for For {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek2(Token![:]) && input.peek(Lifetime) {
            let label: Lifetime = input.parse()?;
            return Err(syn::Error::new(label.span(), "Labels are not supported"));
        }

        let for_token: Token![for] = input.parse()?;
        let pat = input.parse()?;
        let in_token: Token![in] = input.parse()?;
        let expr = input.parse()?;
        let body = input.parse()?;
        Ok(For {
            for_token,
            pat,
            _in_token: in_token,
            expr,
            body,
        })
    }
}

impl Parse for While {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek2(Token![:]) && input.peek(Lifetime) {
            let label: Lifetime = input.parse()?;
            return Err(syn::Error::new(label.span(), "Labels are not supported"));
        }

        let while_token: Token![while] = input.parse()?;
        let condition = input.parse()?;
        let body = input.parse()?;
        Ok(While {
            while_token,
            condition,
            body,
        })
    }
}

impl Parse for Loop {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek2(Token![:]) && input.peek(Lifetime) {
            let label: Lifetime = input.parse()?;
            return Err(syn::Error::new(label.span(), "Labels are not supported"));
        }

        let loop_token: Token![loop] = input.parse()?;
        let body = input.parse()?;
        Ok(Loop { loop_token, body })
    }
}

impl Parse for Return {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let return_token: Token![return] = input.parse()?;
        let value = if input.peek(Token![;]) {
            None
        } else {
            Some(input.parse()?)
        };
        Ok(Return {
            return_token,
            value,
        })
    }
}

impl Parse for ReturnBind {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let return_token: Token![return] = input.parse()?;
        let question_mark: Token![?] = input.parse()?;
        let value = if input.peek(Token![;]) {
            None
        } else {
            Some(input.parse()?)
        };
        Ok(ReturnBind {
            return_token,
            _question_mark: question_mark,
            value,
        })
    }
}

impl Parse for Yield {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let yield_token: Token![yield] = input.parse()?;
        let value = if input.peek(Token![;]) {
            None
        } else {
            Some(input.parse()?)
        };
        Ok(Yield { yield_token, value })
    }
}

impl Parse for YieldBind {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let yield_token: Token![yield] = input.parse()?;
        let question_mark: Token![?] = input.parse()?;
        let value = if input.peek(Token![;]) {
            None
        } else {
            Some(input.parse()?)
        };
        Ok(YieldBind {
            yield_token,
            _question_mark: question_mark,
            value,
        })
    }
}

impl Parse for CExprStmt {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        match input.peek2(Token![?]) {
            false if input.peek(Brace) => input.parse().map(CExprStmt::Block),
            true if input.peek(Token![let]) => input.parse().map(CExprStmt::LetBind),
            true if input.peek(Token![do]) => input.parse().map(CExprStmt::DoBind),
            false if input.peek(Token![if]) => input.parse().map(CExprStmt::If),
            false if input.peek(Token![match]) => input.parse().map(CExprStmt::Match),
            false if input.peek(Token![for]) => input.parse().map(CExprStmt::For),
            false if input.peek(Token![while]) => input.parse().map(CExprStmt::While),
            false if input.peek(Token![loop]) => input.parse().map(CExprStmt::Loop),
            false if input.peek(Token![return]) => input.parse().map(CExprStmt::Return),
            true if input.peek(Token![return]) => input.parse().map(CExprStmt::ReturnBind),
            false if input.peek(Token![yield]) => input.parse().map(CExprStmt::Yield),
            true if input.peek(Token![yield]) => input.parse().map(CExprStmt::YieldBind),
            _ => {
                input.parse().map(|stmt| {
                    if let Stmt::Expr(expr) = stmt {
                        // Implicit return
                        CExprStmt::ReturnImplicit(expr)
                    } else {
                        // Normal statement
                        CExprStmt::Normal(stmt)
                    }
                })
            }
        }
    }
}

impl Parse for CExprStmts {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut stmts = Vec::new();
        while !input.is_empty() {
            // Skip over semicolons, they don't really matter
            while input.parse::<Option<Token![;]>>()?.is_some() {}
            if input.is_empty() {
                break;
            }

            // Parse a statement
            let stmt: CExprStmt = input.parse()?;
            stmts.push(stmt);
        }

        Ok(CExprStmts { stmts })
    }
}

impl Parse for ExecuteCExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_kvs! {
            input,
            required = {
                defs: Braced<FunctionDefs>,
                expr: Braced<CExprStmts>,
            },
            optional = {}
        }

        Ok(ExecuteCExpr {
            defs: defs.inner,
            expr: expr.inner,
        })
    }
}
