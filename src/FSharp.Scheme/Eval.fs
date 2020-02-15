namespace FSharp.Scheme.Core

module Eval =
    open System
    open FSharp.Scheme.Core.Ast
    open FSharp.Scheme.Core.Env
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

    let car (ls: LispVal list): LispVal =
        match ls with
        | [ List(x :: xs) ] -> x
        | [ DottedList(x :: xs, _) ] -> x
        | [ badarg ] -> raise <| TypeMismatchException("pair", badarg)
        | _ -> raise <| NumArgsException(1, List.length ls, ls)

    let cdr (ls: LispVal list): LispVal =
        match ls with
        | [ List(x :: xs) ] -> List xs
        | [ DottedList([ xs ], x) ] -> x
        | [ DottedList(_ :: xs, x) ] -> DottedList(xs, x)
        | [ badarg ] -> raise <| TypeMismatchException("pair", badarg)
        | _ -> raise <| NumArgsException(1, List.length ls, ls)

    let cons (ls: LispVal list): LispVal =
        match ls with
        | [ x; List xs ] -> List(x :: xs)
        | [ x; DottedList(head, last) ] -> DottedList(x :: head, last)
        | [ x; y ] -> DottedList([ x ], y) // 2つの非リスト
        | _ -> raise <| NumArgsException(2, List.length ls, ls)

    let rec internal eqvCore (ls: LispVal list): bool =
        match ls with
        | [ Bool arg1; Bool arg2 ] -> arg1 = arg2
        | [ Integer arg1; Integer arg2 ] -> arg1 = arg2
        | [ Float arg1; Float arg2 ] -> arg1 = arg2
        | [ String arg1; String arg2 ] -> arg1 = arg2
        | [ Atom arg1; Atom arg2 ] -> arg1 = arg2
        | [ DottedList(xs, x); DottedList(ys, y) ] ->
            let l = List(xs @ [ x ])
            let r = List(ys @ [ y ])
            eqvCore [ l; r ]
        | [ List arg1; List arg2 ] -> List.length arg1 = List.length arg2 && eqvPair arg1 arg2
        | [ _; _ ] -> false // 型が違う
        | _ -> raise <| NumArgsException(2, List.length ls, ls)

    and internal eqvPair xs ys =
        match xs, ys with
        | [], [] -> true // 各要素が等しく、要素数も等しい
        | x :: xs, y :: ys ->
            if eqvCore [ x; y ] then eqvPair xs ys else false
        | _ -> false

    let eqv x = Bool(eqvCore x)

    let rec equal x =
        let comp op x y =
            try
                op x = op y
            with TypeMismatchException(_) -> false
        match x with
        | [ xs; ys ] ->
            let b = eqvCore x || comp unpackInt xs ys || comp unpackStr xs ys || comp unpackBool xs ys
            Bool b
        | _ -> raise <| NumArgsException(2, List.length x, x)

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
        |> Map.add "car" car
        |> Map.add "cdr" cdr
        |> Map.add "cons" cons
        |> Map.add "eq?" eqv
        |> Map.add "eqv?" eqv
        |> Map.add "equal?" equal

    let apply (func: string) (args: LispVal list): LispVal =
        match Map.tryFind func primitives with
        | Some f -> f args
        | None -> raise <| NotFunctionException("Unrecognized primitive function args", func)

    let rec eval (env: Env) (x: LispVal): LispVal =
        match x with
        | String _
        | Integer _
        | Float _
        | Bool _ -> x
        | Atom id -> Env.getVar env id
        | List [ Atom "quote"; ls ] -> ls // クォート外し
        | List [ Atom "if"; pred; conseq; alt ] ->
            match eval env pred with
            | Bool true -> eval env conseq
            | Bool false -> eval env alt
            | _ -> raise <| BadSpecialFormException("if form should take boolean prediction", x)
        | List [ Atom "set!"; Atom var; form ] ->
            form
            |> eval env
            |> Env.setVar env var
        | List [ Atom "define"; Atom var; form ] ->
            form
            |> eval env
            |> Env.defineVar env var
        | List(Atom func :: args) ->
            args
            |> List.map (eval env)
            |> apply func
        | _ -> raise <| BadSpecialFormException("Unrecognized special form", x)
