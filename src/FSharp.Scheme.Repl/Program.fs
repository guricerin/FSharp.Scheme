module FSharp.Scheme.Repl

open System
open FSharp.Scheme.Core.Parsing

let rec loop() =
    printf "> "
    stdin.ReadLine()
    |> Parsing.readExpr
    |> printfn "%s"
    loop()

[<EntryPoint>]
let main argv =
    printfn "try"
    loop()
    0 // return an integer exit code
