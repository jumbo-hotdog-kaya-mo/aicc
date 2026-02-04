type Ast = Vec<TopLevelDecl>;
type BlockStmt = Vec<Stmt>;
type BlockExpr = (Vec<Stmt>, Expr);

enum Block {
    Expr(BlockExpr),
    Stmt(BlockStmt)
}

enum Expr {
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Block(BlockExpr),
    Call(String, Vec<Expr>),
    AssignList(Vec<(String, Expr)>, BlockExpr),
    MakeColor(Vec<Expr>),
    List(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
    Rvalue(Rvalue),
    Literal(Literal)
}

enum Lvalue {
    Ident(String),
    Global(String),
    ComponentProp(String, String, String)
}

enum Rvalue {
    Component(String, String),
    ComponentType(String),
    Lvalue(Lvalue)
}

enum Stmt {
    AssignList(Vec<(String, Expr)>, BlockStmt),
    Call(String, Vec<Expr>),
    For(For),
    If(Vec<(Expr, BlockStmt)>, Option<BlockStmt>),
    Modify(Lvalue, Expr),
    While(Expr, BlockStmt),
}

enum TopLevelDecl {
    Global(String, Expr),
    When(String, Option<String>, String, Block),
    Func(String, Vec<String>, Block)
}
