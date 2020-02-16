namespace FSharp.Scheme.Core

module Io =
    open System
    open System.IO
    open FSharp.Scheme.Core.Types
    open FSharp.Scheme.Core.Errors
    open FSharp.Scheme.Core.Ast
    open FSharp.Scheme.Core.Parsing

    [<RequireQualifiedAccess>]
    module PortIn =

        let init =
            function
            | [ String s ] -> PortIn(new StreamReader(s))
            | x -> raise <| TypeMismatchException("String", List x)

        let close =
            function
            | [ PortIn p ] ->
                p.Dispose()
                Bool true
            | _ -> Bool false

        let rec readProc =
            function
            | [] -> readProc [ PortIn(new StreamReader(Console.OpenStandardInput())) ]
            | [ PortIn p ] ->
                let line = p.ReadLine()
                if isNull line then "'()" else line
                |> readExpr
            | x -> raise <| TypeMismatchException("PortIn", List x)

        let read (filename: string) =
            use reader = new StreamReader(filename)

            let rec loop acc =
                match reader.ReadLine() with
                | null ->
                    acc
                    |> List.rev
                    |> fun s -> String.Join("\n", s)
                | s -> loop (s :: acc)
            loop []

        let readContents =
            function
            | [ String filename ] -> String(read filename)
            | x -> raise <| TypeMismatchException("String", List x)

        let readAll =
            function
            | [ String filename ] ->
                filename
                |> read
                |> sprintf "(%s)"
                |> readExpr
            | x -> raise <| TypeMismatchException("String", List x)

        let load (filename: string) =
            match readAll [ String filename ] with
            | List xs -> xs
            | x -> raise <| DefaultException("readAll returned non-list")

    [<RequireQualifiedAccess>]
    module PortOut =

        let init =
            function
            | [ String s ] -> PortOut(new StreamWriter(s))
            | x -> raise <| TypeMismatchException("String", List x)

        let close =
            function
            | [ PortOut p ] ->
                p.Dispose()
                Bool true
            | _ -> Bool false

        let rec writeProc =
            function
            | [ x ] ->
                writeProc
                    [ x
                      PortOut(new StreamWriter(Console.OpenStandardOutput())) ]
            | [ x; PortOut p ] ->
                x
                |> LispVal.toString
                |> p.WriteLine
                Bool true
            | x -> raise <| TypeMismatchException("PortOut", List x)
