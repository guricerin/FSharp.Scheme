namespace FSharp.Scheme.Core

module Primitives =
    open System
    open FSharp.Scheme.Core.Types
    open FSharp.Scheme.Core.Env
    open FSharp.Scheme.Core.Errors
    open FSharp.Scheme.Core.Io

    /// LispValをintに型変換
    let rec internal unpackInt x =
        match x with
        | Integer n -> n
        | String s ->
            match Int32.TryParse(s) with
            | true, n -> n
            | false, _ -> raise <| TypeMismatchException("Integer", x)
        | List [ l ] -> unpackInt l // 1要素の場合のみ許容
        | _ -> raise <| TypeMismatchException("Integer", x)

    let internal unpackStr x =
        match x with
        | String s -> s
        | Integer n -> sprintf "%d" n
        | Bool b -> sprintf "%b" b
        | _ -> raise <| TypeMismatchException("String", x)

    let internal unpackBool x =
        match x with
        | Bool b -> b
        | _ -> raise <| TypeMismatchException("Bool", x)

    /// 二項演算をfold
    /// etc. (+ a b c ...)
    let internal numericBinop (op: int -> int -> int) (args: LispVal list): LispVal =
        match args with
        | [] -> raise <| NumArgsException(2, 0, [ String "zero arg" ])
        | [ x ] -> raise <| NumArgsException(2, 1, [ x ])
        | _ ->
            args
            |> List.map unpackInt
            |> List.reduce op
            |> Integer

    let internal boolBinop (unpacker: LispVal -> 'a) (op: 'a -> 'a -> bool) (args: LispVal list): LispVal =
        match args with
        | [ l; r ] ->
            let left, right = unpacker l, unpacker r
            Bool(op left right)
        | _ ->
            let n = List.length args
            raise <| NumArgsException(2, n, args)

    let internal intBoolBinop = boolBinop unpackInt
    let internal strBoolBinop = boolBinop unpackStr
    let internal boolBoolBinop = boolBinop unpackBool

    let internal car (ls: LispVal list): LispVal =
        match ls with
        | [ List(x :: xs) ] -> x
        | [ DottedList(x :: xs, _) ] -> x
        | [ badarg ] -> raise <| TypeMismatchException("pair", badarg)
        | _ -> raise <| NumArgsException(1, List.length ls, ls)

    let internal cdr (ls: LispVal list): LispVal =
        match ls with
        | [ List(x :: xs) ] -> List xs
        | [ DottedList([ xs ], x) ] -> x
        | [ DottedList(_ :: xs, x) ] -> DottedList(xs, x)
        | [ badarg ] -> raise <| TypeMismatchException("pair", badarg)
        | _ -> raise <| NumArgsException(1, List.length ls, ls)

    let internal cons (ls: LispVal list): LispVal =
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

    let internal eqv x = Bool(eqvCore x)

    let rec internal equal x =
        let comp op x y =
            try
                op x = op y
            with TypeMismatchException(_) -> false
        match x with
        | [ xs; ys ] ->
            let b = eqvCore x || comp unpackInt xs ys || comp unpackStr xs ys || comp unpackBool xs ys
            Bool b
        | _ -> raise <| NumArgsException(2, List.length x, x)

    [<RequireQualifiedAccess>]
    module Primitives =
        let init() =
            let add varname body env =
                Env.defineVar env varname (PrimitiveFunc body) |> ignore
                env
            Env.init
            |> add "+" (numericBinop (+))
            |> add "-" (numericBinop (-))
            |> add "*" (numericBinop (*))
            |> add "/" (numericBinop (/))
            |> add "%" (numericBinop (%))
            |> add "=" (intBoolBinop (=))
            |> add "/=" (intBoolBinop (<>))
            |> add "<" (intBoolBinop (<))
            |> add ">" (intBoolBinop (>))
            |> add "<=" (intBoolBinop (<=))
            |> add ">=" (intBoolBinop (>=))
            |> add "&&" (boolBoolBinop (&&))
            |> add "||" (boolBoolBinop (||))
            |> add "string=?" (strBoolBinop (=))
            |> add "string<?" (strBoolBinop (<))
            |> add "string>?" (strBoolBinop (>))
            |> add "string<=?" (strBoolBinop (<=))
            |> add "string>=?" (strBoolBinop (>=))
            |> add "car" car
            |> add "cdr" cdr
            |> add "cons" cons
            |> add "eq?" eqv
            |> add "eqv?" eqv
            |> add "equal?" equal
            |> add "open-input-file" PortIn.init
            |> add "open-output-file" PortOut.init
            |> add "close-input-file" PortIn.close
            |> add "close-output-file" PortOut.close
            |> add "read" PortIn.readProc
            |> add "write" PortOut.writeProc
            |> add "read-contents" PortIn.readContents
            |> add "read-all" PortIn.readAll
