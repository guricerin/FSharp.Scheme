namespace FSharp.Scheme.Core.Ast

module Ast =
    open System

    type LispVal =
        | Atom of string
        | List of LispVal list
        | DottedList of LispVal list * LispVal
        | Integer of int
        | Float of float
        | String of string
        | Bool of bool

    [<RequireQualifiedAccess>]
    module LispVal =
        let rec toString lv =
            match lv with
            | String s -> sprintf "\"%s\"" s
            | Atom name -> name
            | Integer i -> string i
            | Float f -> string f
            | Bool true -> "#t"
            | Bool false -> "#f"
            | List ls -> sprintf "(%s)" (unwordsList ls)
            | DottedList(head, tail) -> sprintf "(%s . %s)" (unwordsList head) (toString tail)

        and unwordsList contents =
            contents
            |> List.map toString
            |> fun xs -> String.Join(" ", xs)
