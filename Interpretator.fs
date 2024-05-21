module Interpretator

open AST

let rec eval env expr = 
    match expr with
    | Expr.Num(n) -> Expr.Num(n), env
    | Expr.Bool(b) -> Expr.Bool(b), env
    | Expr.Id(id) -> 
        let value = Map.find id env
        value, env
    | Expr.Func(name, parameters, body, _, arity) ->
        let updatedEnv = Map.add name (Expr.Func(name, parameters, body, env, arity)) env
        Expr.Bool(true), updatedEnv  // Возвращаем Bool просто для обозначения успешного определения функции
    | Expr.Call(funcName, Expr.SimpleList(arguments), arity) ->
        let Expr.Func(_, Expr.SimpleArgs(params), body, funcEnv, _) = Map.find funcName env
        let updatedEnv = List.fold2 (fun accEnv param arg -> 
            let argValue, _ = eval env arg
            Map.add param argValue accEnv
        ) funcEnv params arguments
        eval updatedEnv body
    | Expr.Op(op, operands) ->
        let evaluatedOperands = List.map (fun e -> fst (eval env e)) operands
        let result = match op, evaluatedOperands with
            | "+", [Expr.Num(x); Expr.Num(y)] -> Expr.Num(x + y)
            | "-", [Expr.Num(x); Expr.Num(y)] -> Expr.Num(x - y)
            | "*", [Expr.Num(x); Expr.Num(y)] -> Expr.Num(x * y)
            | "/", [Expr.Num(x); Expr.Num(y)] -> Expr.Num(x / y)
            | _ -> failwith "eval ERROR: Unsupported operator or operand types"
        result, env
    | Expr.Cond(condition, thenExpr, elseExpr) ->
        let Expr.Bool(condResult), _ = eval env condition
        if condResult then eval env thenExpr else eval env elseExpr
