use bitflags::bitflags;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use syn::{Expr, Pat, Stmt, Token, Type};

macro_rules! modifiers {
    ($name:ident : $inner_ty:ty = [$(($mod_name:ident, $val:expr, $token:expr)),* $(,)?]) => {
        bitflags! {
            #[derive(Default)]
            pub struct $name: $inner_ty {
                const NONE = 0;
                $(const $mod_name = $val;)*
            }
        }

        impl ToTokens for $name {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                #[allow(unused_mut)]
                let mut modifiers = Vec::<TokenStream>::new();
                $(
                    if self.contains(Self::$mod_name) {
                        modifiers.push($token.to_token_stream());
                    }
                )*
                tokens.append_separated(modifiers, ", ");
            }
        }
    };
}

#[derive(Clone, Debug)]
pub struct Braced<T> {
    pub inner: T,
}

#[derive(Clone)]
pub struct FunctionDefs {
    pub bind: Option<Modified<BindModifiers, Expr>>,
    pub delay: Option<Modified<DelayModifiers, Expr>>,
    pub combine: Option<Modified<CombineModifiers, Expr>>,
    pub then: Option<Modified<ThenModifiers, Expr>>,
    pub r#else: Option<Modified<ElseModifiers, Expr>>,
    pub r#for: Option<Modified<ForModifiers, Expr>>,
    pub r#while: Option<Modified<WhileModifiers, Expr>>,
    pub r#loop: Option<Modified<LoopModifiers, Expr>>,
    pub r#return: Option<Modified<ReturnModifiers, Expr>>,
    pub r#yield: Option<Modified<YieldModifiers, Expr>>,
    pub zero: Option<Modified<ZeroModifiers, Expr>>,
    pub run: Option<Modified<RunModifiers, Expr>>,
}

#[derive(Clone, Debug)]
pub struct Modified<M, T> {
    pub modifiers: M,
    pub modifiers_span: Option<Span>,
    pub inner: T,
}

impl<M: ToTokens, T: ToTokens> ToTokens for Modified<M, T> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.inner.to_tokens(tokens);
    }
}

modifiers!(BindModifiers: u8 = [(MOVE, 0b1, <Token![move]>::default())]);
modifiers!(DelayModifiers: u8 = [(MOVE, 0b1, <Token![move]>::default())]);
modifiers!(CombineModifiers: u8 = []);
modifiers!(ThenModifiers: u8 = []);
modifiers!(ElseModifiers: u8 = []);
modifiers!(ForModifiers: u8 = [(MOVE, 0b1, <Token![move]>::default())]);
modifiers!(WhileModifiers: u8 = [(MOVE, 0b1, <Token![move]>::default())]);
modifiers!(LoopModifiers: u8 = []);
modifiers!(ReturnModifiers: u8 = []);
modifiers!(YieldModifiers: u8 = []);
modifiers!(ZeroModifiers: u8 = []);
modifiers!(RunModifiers: u8 = []);

#[derive(Clone)]
pub struct LetBind {
    pub _let_token: Token![let],
    pub _question_token: Token![?],
    pub target: Pat,
    pub ty: Option<(Token![:], Type)>,
    pub _eq_token: Token![=],
    pub value: Expr,
}

#[derive(Clone)]
pub struct DoBind {
    pub _do_token: Token![do],
    pub _question_token: Token![?],
    pub value: Expr,
}

#[derive(Clone)]
pub struct If {
    pub if_token: Token![if],
    pub condition: Expr,
    pub then: Braced<CExprStmts>,
    pub r#else: Option<Else>,
}

#[derive(Clone)]
pub enum Else {
    ElseIf(Token![else], Box<If>),
    Else(Token![else], Braced<CExprStmts>),
}

#[derive(Clone)]
pub struct Match {
    pub match_token: Token![match],
    pub expr: Expr,
    pub arms: Braced<MatchArms>,
}

#[derive(Clone)]
pub struct MatchArms {
    pub arms: Vec<MatchArm>,
}

#[derive(Clone)]
pub struct MatchArm {
    pub pat: Pat,
    pub guard: Option<(Token![if], Expr)>,
    pub _fat_arrow_token: Token![=>],
    pub body: CExprStmt,
    pub _comma: Option<Token![,]>,
}

#[derive(Clone)]
pub struct For {
    pub for_token: Token![for],
    pub pat: Pat,
    pub _in_token: Token![in],
    pub expr: Expr,
    pub body: Braced<CExprStmts>,
}

#[derive(Clone)]
pub struct While {
    pub while_token: Token![while],
    pub condition: Expr,
    pub body: Braced<CExprStmts>,
}

#[derive(Clone)]
pub struct Loop {
    pub loop_token: Token![loop],
    pub body: Braced<CExprStmts>,
}

#[derive(Clone)]
pub struct Return {
    pub return_token: Token![return],
    pub value: Option<Expr>,
}

#[derive(Clone)]
pub struct ReturnBind {
    pub return_token: Token![return],
    pub _question_mark: Token![?],
    pub value: Option<Expr>,
}

#[derive(Clone)]
pub struct Yield {
    pub yield_token: Token![yield],
    pub value: Option<Expr>,
}

#[derive(Clone)]
pub struct YieldBind {
    pub yield_token: Token![yield],
    pub _question_mark: Token![?],
    pub value: Option<Expr>,
}

#[derive(Clone)]
pub enum CExprStmt {
    Normal(Stmt),
    Block(Braced<CExprStmts>),
    LetBind(LetBind),
    DoBind(DoBind),
    If(If),
    Match(Match),
    For(For),
    While(While),
    Loop(Loop),
    Return(Return),
    ReturnBind(ReturnBind),
    ReturnImplicit(Expr),
    Yield(Yield),
    YieldBind(YieldBind),
}

#[derive(Clone)]
pub struct CExprStmts {
    pub stmts: Vec<CExprStmt>,
}

#[derive(Clone)]
pub struct ExecuteCExpr {
    pub defs: FunctionDefs,
    pub expr: CExprStmts,
}
