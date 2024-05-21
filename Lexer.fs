module Lexer

type Token =
    | Op of string
    | Num of float
    | Str of string
    | Id of string
    | LParen
    | RParen

let tokenize source =
    let tokens = [
        ('(', Token.LParen)
        (')', Token.RParen)
        ('+', Token.Op("+"))
        ('-', Token.Op("-"))
        ('*', Token.Op("*"))
        ('/', Token.Op("/"))
        ('=', Token.Op("="))
    ]

    let tokenMap = Map.ofList tokens

    let rec readString acc = function
        | '\\'::'"'::t -> (String(List.rev acc)), t
        | '"'::t -> (String(List.rev acc)), t
        | h::t -> readString (h::acc) t
        | [] -> failwith "readString ERROR: EOF before closing \" found"

    let rec readId acc = function
        | h::t when Char.IsLetterOrDigit(h) || h = '_' -> readId (h::acc) t
        | t -> (String(List.rev acc)), t

    let rec readNum acc = function
        | h::t when Char.IsDigit(h) -> readNum (h::acc) t
        | '.'::t -> readNum ('.'::acc) t
        | t -> (String(List.rev acc)), t

    let rec lexer acc = function
        | [] -> List.rev acc
        | h::t when Char.IsWhiteSpace(h) -> lexer acc t
        | '"'::t -> 
            let str, remaining = readString [] t
            lexer (Token.Str(str)::acc) remaining
        | h::t when tokenMap |> Map.containsKey h -> lexer ((tokenMap |> Map.find h)::acc) t
        | h::t when Char.IsLetter(h) ->
            let id, remaining = readId [h] t
            lexer (Token.Id(id)::acc) remaining
        | h::t when Char.IsDigit(h) ->
            let numStr, remaining
