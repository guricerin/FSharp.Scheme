namespace FSharp.Scheme.Core

module Eval =
    open System
    open FSharp.Scheme.Core.Ast

    /// LispValをintに型変換
    let rec unpackNum lv =
        match lv with
        | Integer n -> n
        | String s ->
            match Int32.TryParse(s) with
            | true, n -> n
            | false, _ -> 0
        | List ls ->
            match ls with
            | [ l ] -> unpackNum l // 1要素の場合のみ許容
            | _ -> 0
        | _ -> 0

    /// 二項演算をfold
    let numericBinop (op: int -> int -> int) (args: LispVal list) =
        args
        |> List.map unpackNum
        |> List.reduce op
        |> Integer

    let inline internal rem x y =
        let zero = LanguagePrimitives.GenericZero
        let res = x % y
        if res < zero then res + y else res

    let primitives =
        [ "+", numericBinop (+)
          "-", numericBinop (-)
          "*", numericBinop (*)
          // 除算（-∞ 方向に丸め）
          "/", numericBinop (/)
          // 剰余（div の余り）
          "mod", numericBinop (%)
          // 除算（ゼロ方向に丸め）
          "quotient", numericBinop (/)
          // 剰余（quot の余り）
          "remainder", numericBinop (%) ]
        |> Map.ofList

    let apply (func: string) (args: LispVal list): LispVal =
        match Map.tryFind func primitives with
        | Some f -> f args
        | None -> Bool false

    let rec eval lv =
        match lv with
        | String _
        | Integer _
        | Float _
        | Bool _ -> lv
        | List [ Atom "quote"; ls ] -> ls // クォート外し
        | List(Atom func :: args) ->
            args
            |> List.map eval
            |> apply func
