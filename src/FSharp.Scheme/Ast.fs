namespace FSharp.Scheme.Core

module Ast =
    open System
    open Types

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
            | PrimitiveFunc _ -> "<primitive>"
            | Func func ->
                let param = String.Join(" ", func.param)

                let vararg =
                    match func.vararg with
                    | Some(a) -> sprintf " . %s" a
                    | None -> ""
                sprintf "(lambda (%s%s) ...)" param vararg

        and unwordsList contents =
            contents
            |> List.map toString
            |> fun xs -> String.Join(" ", xs)

        let isAtom =
            function
            | Atom _ -> true
            | _ -> false

        let isString =
            function
            | String _ -> true
            | _ -> false

        let isInteger =
            function
            | Integer _ -> true
            | _ -> false

        let isFloat =
            function
            | Float _ -> true
            | _ -> false

        let isNumber lv = isInteger lv || isFloat lv

        let isBool =
            function
            | Bool _ -> true
            | _ -> false

        let isList =
            function
            | List _
            | DottedList _ -> true
            | _ -> false
