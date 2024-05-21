module AST

type Expr =
    | Op of string * Expr list
    | Num of float
    | Str of string
    | Id of string
    | Bool of bool
    | Cond of Expr * Expr * Expr
    | Var of string * Expr
    | Set of string * Expr
    | Func of string * Expr * Expr * Env * int
    | Call of string * Expr * int
    | Print of Expr
    | Simple of string
    | SimpleOp of string
    | SimpleList of Expr list
    | SimpleArgs of Expr list
and Env = Map<string, Expr>
