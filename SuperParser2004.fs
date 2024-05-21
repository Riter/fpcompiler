module SuperParser2004

open Lexer
open AST

let parse tokens = 
    let keywords = ["var"; "put"; "def"; "sout"; "if"; "then"; "else"]

    let rec parseExprs acc = function
        | [] -> List.rev acc, []
        | Token.Id(id)::t when List.contains id keywords -> parseExprs (Expr.Simple(id)::acc) t
        | Token.Num(n)::t -> parseExprs (Expr.Num(n)::acc) t
        | Token.Id("true")::t -> parseExprs (Expr.Bool(true)::acc) t
        | Token.Id("false")::t -> parseExprs (Expr.Bool(false)::acc) t
        | Token.Id(id)::t -> parseExprs (Expr.Id(id)::acc) t
        | Token.Str(s)::t -> parseExprs (Expr.Str(s)::acc) t
        | Token.LParen::t ->
            let innerExprs, rest = parseExprs [] t
            parseExprs (Expr.SimpleList(innerExprs)::acc) rest
        | Token.RParen::t -> List.rev acc, t
        | Token.Op(op)::t -> parseExprs (Expr.SimpleOp(op)::acc) t
        | unexpected -> failwithf "Unexpected token: %A" unexpected

    let rec buildAst = function
        | [Expr.Simple("def"); Expr.Id(name); Expr.SimpleArgs(args); body] ->
            Expr.Func(name, Expr.SimpleArgs(args), body, Map.empty, List.length args)
        | [Expr.Simple("var"); Expr.Id(name); value] ->
            Expr.Var(name, value)
        | [Expr.Simple("put"); Expr.Id(name); value] ->
            Expr.Set(name, value)
        | [Expr.Simple("sout"); value] ->
            Expr.Print(value)
        | [Expr.Simple("if"); cond; Expr.Simple("then"); thenExpr; Expr.Simple("else"); elseExpr] ->
            Expr.Cond(cond, thenExpr, elseExpr)
        | [Expr.Simple("if"); cond; Expr.Simple("then"); thenExpr] ->
            Expr.Cond(cond, thenExpr, Expr.Simple(""))
        | [Expr.Id(name); args] when List.forall (function Expr.SimpleList(_) -> true | _ -> false) args ->
            Expr.Call(name, Expr.SimpleList(args), List.length args)
        | [Expr.SimpleOp(op); args] ->
            Expr.Op(op, args)
        | [Expr.Num(n)] -> Expr.Num(n)
        | [Expr.Str(s)] -> Expr.Str(s)
        | [Expr.Bool(b)] -> Expr.Bool(b)
        | _ -> failwith "Invalid expression structure"

    let parsedExprs, remainingTokens = parseExprs [] tokens
    if remainingTokens <> [] then failwith "Parsing error: unprocessed tokens remain"
    Expr.SimpleList(List.map buildAst parsedExprs)
