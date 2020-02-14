module FSharp.Scheme.Repl

open System
open FSharp.Scheme.Core.Parsing
open FSharp.Scheme.Core.Eval

let rec loop() =
    printf "\n> "
    stdin.ReadLine()
    |> Parsing.readExpr
    |> Eval.eval
    |> printfn "%A"
    loop()

[<EntryPoint>]
let main argv =
    printfn "Welcom Debug Room"
    loop()
    0 // return an integer exit code
