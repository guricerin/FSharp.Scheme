module FSharp.Scheme.Repl

open System

[<EntryPoint>]
let main argv =
    if Array.length argv < 2 then
        let msg = sprintf "Error: Should take 2 commandline arguments."
        printfn "%s" msg
    else
        let a = Convert.ToInt32(argv.[0])
        let b = Convert.ToInt32(argv.[1])
        let c = a + b
        printfn "argv[0] + argv[1] = %d" c
        printf "input your name. >"
        let name = stdin.ReadLine()
        printfn "Hello, %s" name
    0 // return an integer exit code
