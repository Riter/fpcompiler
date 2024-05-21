module Program

open Lexer
open SuperParser2004
open Interpretator
open System

let run code =
    try
        let tokens = Lexer.tokenize (List.ofSeq code)
        let ast = SuperParser2004.parse tokens
        let result, _ = Interpretator.eval Map.empty ast
        printfn "Result: %A\n" result
    with ex ->
        printfn "Error: %s" ex.Message

[<EntryPoint>]
let main _ =
    printfn "Enter the source code:"
    let code = Console.ReadLine()
    run code
    0
