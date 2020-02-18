module FSharp.Scheme.Repl

open System
open FSharp.Scheme.Core
open FSharp.Scheme.Core.Ast
open FSharp.Scheme.Core.Env
open FSharp.Scheme.Core.Primitives
open FSharp.Scheme.Core.Errors

let printPrompt() =
    printf "\nLisp>>> "

let gEnv = Primitives.init()

let rep input =
    try
        input
        |> Parsing.readExpr
        |> Eval.eval gEnv
        |> LispVal.toString
        |> printfn "%s"
    with ex ->
        ex
        |> LispError.toString
        |> printfn "%s"

let rec repl() =
    printPrompt()
    match stdin.ReadLine() with
    | "" -> repl()
    | "quit" ->
        printfn "good bye!"
        ()
    | input ->
        rep input
        repl()

[<EntryPoint>]
let main argv =
    match Array.length argv with
    | 0 ->
        printfn "Welcom Repl Room"
        repl()
    | 1 -> rep argv.[0]
    | _ -> printfn "Program takes only 0 or 1 argument"
    0 // return an integer exit code
