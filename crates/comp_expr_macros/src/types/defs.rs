use syn::{Expr, Pat, Stmt, Token, Type};

pub struct Braced<T> {
    pub inner: T,
}

pub struct FunctionDefs {
    pub bind: Option<Expr>,
    pub delay: Option<Expr>,
    pub combine: Option<Expr>,
    pub r#for: Option<Expr>,
    pub r#while: Option<Expr>,
    pub r#loop: Option<Expr>,
    pub r#return: Option<Expr>,
    pub r#yield: Option<Expr>,
    pub zero: Option<Expr>,
    pub run: Option<Expr>,
}

pub struct LetBind {
    pub _let_token: Token![let],
    pub _question_token: Token![?],
    pub target: Pat,
    pub ty: Option<(Token![:], Type)>,
    pub _eq_token: Token![=],
    pub value: Expr,
}

pub struct DoBind {
    pub _do_token: Token![do],
    pub _question_token: Token![?],
    pub value: Expr,
}

pub struct If {
    pub if_token: Token![if],
    pub condition: Expr,
    pub then: CExprStmts,
    pub r#else: Option<Else>,
}

pub enum Else {
    ElseIf(Token![else], Box<If>),
    Else(Token![else], Braced<CExprStmts>),
}

pub struct Match {
    pub match_token: Token![match],
    pub expr: Expr,
    pub arms: Braced<MatchArms>,
}

pub struct MatchArms {
    pub arms: Vec<MatchArm>,
}

pub struct MatchArm {
    pub pat: Pat,
    pub guard: Option<(Token![if], Expr)>,
    pub _fat_arrow_token: Token![=>],
    pub body: CExprStmt,
    pub _comma: Option<Token![,]>,
}

pub struct For {
    pub for_token: Token![for],
    pub pat: Pat,
    pub _in_token: Token![in],
    pub expr: Expr,
    pub body: Braced<CExprStmts>,
}

pub struct While {
    pub while_token: Token![while],
    pub condition: Expr,
    pub body: Braced<CExprStmts>,
}

pub struct Loop {
    pub loop_token: Token![loop],
    pub body: Braced<CExprStmts>,
}

pub struct Return {
    pub return_token: Token![return],
    pub value: Option<Expr>,
}

pub struct ReturnBind {
    pub return_token: Token![return],
    pub _question_mark: Token![?],
    pub value: Option<Expr>,
}

pub struct Yield {
    pub yield_token: Token![yield],
    pub value: Option<Expr>,
}

pub struct YieldBind {
    pub yield_token: Token![yield],
    pub _question_mark: Token![?],
    pub value: Option<Expr>,
}

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

pub struct CExprStmts {
    pub stmts: Vec<CExprStmt>,
}

pub struct ExecuteCExpr {
    pub defs: FunctionDefs,
    pub expr: CExprStmts,
}
