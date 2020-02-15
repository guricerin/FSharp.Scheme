namespace FSharp.Scheme.Core

module Eval =
    open System
    open FSharp.Scheme.Core.Ast
    open FSharp.Scheme.Core.Errors

    /// LispValをintに型変換
    let rec unpackInt x =
        match x with
        | Integer n -> n
        | String s ->
            match Int32.TryParse(s) with
            | true, n -> n
            | false, _ -> raise <| TypeMismatchException("Integer", x)
        | List [ l ] -> unpackInt l // 1要素の場合のみ許容
        | _ -> raise <| TypeMismatchException("Integer", x)

    let unpackStr x =
        match x with
        | String s -> s
        | Integer n -> sprintf "%d" n
        | Bool b -> sprintf "%b" b
        | _ -> raise <| TypeMismatchException("String", x)

    let unpackBool x =
        match x with
        | Bool b -> b
        | _ -> raise <| TypeMismatchException("Bool", x)

    /// 二項演算をfold
    /// etc. (+ a b c ...)
    let numericBinop (op: int -> int -> int) (args: LispVal list): LispVal =
        match args with
        | [] -> raise <| NumArgsException(2, 0, [ String "zero arg" ])
        | [ x ] -> raise <| NumArgsException(2, 1, [ x ])
        | _ ->
            args
            |> List.map unpackInt
            |> List.reduce op
            |> Integer

    let boolBinop (unpacker: LispVal -> 'a) (op: 'a -> 'a -> bool) (args: LispVal list): LispVal =
        match args with
        | [ l; r ] ->
            let left, right = unpacker l, unpacker r
            Bool(op left right)
        | _ ->
            let n = List.length args
            raise <| NumArgsException(2, n, args)

    let intBoolBinop = boolBinop unpackInt
    let strBoolBinop = boolBinop unpackStr
    let boolBoolBinop = boolBinop unpackBool

    let primitives =
        Map.empty
        |> Map.add "+" (numericBinop (+))
        |> Map.add "-" (numericBinop (-))
        |> Map.add "*" (numericBinop (*))
        |> Map.add "/" (numericBinop (/))
        |> Map.add "%" (numericBinop (%))
        |> Map.add "=" (intBoolBinop (=))
        |> Map.add "/=" (intBoolBinop (<>))
        |> Map.add "<" (intBoolBinop (<))
        |> Map.add ">" (intBoolBinop (>))
        |> Map.add "<=" (intBoolBinop (<=))
        |> Map.add ">=" (intBoolBinop (>=))
        |> Map.add "&&" (boolBoolBinop (&&))
        |> Map.add "||" (boolBoolBinop (||))
        |> Map.add "string=?" (strBoolBinop (=))
        |> Map.add "string<?" (strBoolBinop (<))
        |> Map.add "string>?" (strBoolBinop (>))
        |> Map.add "string<=?" (strBoolBinop (<=))
        |> Map.add "string>=?" (strBoolBinop (>=))

    let apply (func: string) (args: LispVal list): LispVal =
        match Map.tryFind func primitives with
        | Some f -> f args
        | None -> raise <| NotFunctionException("Unrecognized primitive function args", func)

    let rec eval (x: LispVal): LispVal =
        match x with
        | String _
        | Integer _
        | Float _
        | Bool _ -> x
        | List [ Atom "quote"; ls ] -> ls // クォート外し
        | List(Atom func :: args) ->
            args
            |> List.map eval
            |> apply func
        | _ -> raise <| BadSpecialFormException("Unrecognized special form", x)
