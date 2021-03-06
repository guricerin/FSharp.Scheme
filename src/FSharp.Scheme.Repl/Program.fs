﻿module FSharp.Scheme.Repl

open FSharp.Scheme.Core
open FSharp.Scheme.Core.Types
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

let runOne (argv: string array) =
    // schemeプログラムに渡すコマンドライン引数
    let args =
        argv
        |> List.ofArray
        |> List.skip 1
        |> List.map LispVal.String
        |> LispVal.List
        |> fun x -> ("args", x)

    let filename = Array.head argv |> LispVal.String

    let env = Primitives.init() |> fun e -> Env.bindVars e [ args ]

    try
        Eval.eval env
            (List
                [ Atom "load"
                  filename ])
        |> LispVal.toString
        |> printfn "%s"
    with ex ->
        ex
        |> LispError.toString
        |> printfn "%s"

[<EntryPoint>]
let main argv =
    match Array.length argv with
    | 0 ->
        printfn "Welcom Repl Room"
        repl()
    | _ -> runOne argv
    0 // return an integer exit code
