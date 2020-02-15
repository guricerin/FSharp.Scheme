module FSharp.Scheme.Repl

open System
open FSharp.Scheme.Core
open FSharp.Scheme.Core.Ast
open FSharp.Scheme.Core.Errors

let rec loop() =
    printf "\nLisp> "
    let input = stdin.ReadLine()
    try
        input
        |> Parsing.readExpr
        |> Eval.eval
        |> LispVal.toString
        |> printfn "%s"
    with ex ->
        ex
        |> LispError.toString
        |> printfn "%s"
    loop()

[<EntryPoint>]
let main argv =
    printfn "Welcom Debug Room"
    loop()
    0 // return an integer exit code
