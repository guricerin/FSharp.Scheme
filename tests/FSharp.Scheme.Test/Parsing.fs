module FSharp.Scheme.Test.Parsing

open System
open Expecto
open FParsec
open FSharp.Scheme.Core.Ast.Ast
open FSharp.Scheme.Core.Parsing
open Parsing

module ParsingTest =

    let config = { FsCheckConfig.defaultConfig with maxTest = 200 }

    let parseTest input expect =
        let actual = run parseExpr input
        match actual with
        | Success(res, _, _) -> res = expect
        | Failure(msg, _, _) -> failwithf "%s" msg

    [<Tests>]
    let ``parse single integer`` =
        let f (a: int) =
            let s = string a
            let expect = Integer a
            parseTest s expect

        testPropertyWithConfig config "parse single integer" <| f

    // [<Tests>]
    let ``parse single float`` =
        let f (a: float) =
            let s =
                match a with
                | _ when a = infinity -> "Infinity"
                | _ when a = nan -> "NaN"
                | _ when a = Double.NegativeInfinity -> "-Infinity"
                | _ -> string a

            let expect =
                match a with
                | _ when a = infinity -> Atom "infinity"
                | _ when a = nan -> Atom "nan"
                | _ when a = Double.NegativeInfinity -> Atom "-infinity"
                | _ -> Float a

            parseTest s expect

        testPropertyWithConfig config "parse single float" <| f

    let tryParse input =
        match run parseExpr input with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "%s" msg

    [<Tests>]
    let ``parse quote symbol`` =
        test "parse quote symbol" {
            Expect.equal (tryParse "quote") (Atom "quote") "quote"
            let expect =
                List
                    [ Atom "quote"
                      Integer 1
                      Integer 2
                      Integer 3 ]
            Expect.equal (tryParse "(quote 1 2 3)") expect "(quote 1 2 3)"
            let expect =
                List
                    [ Atom "quote"
                      List
                          [ Integer 1
                            Integer 2
                            Integer 3 ] ]
            Expect.equal (tryParse "'(1 2 3)") expect "'(1 2 3)"
        }

    [<Tests>]
    let ``parse dotted list`` =
        test "parse dotted list" {
            let expect = DottedList([ Integer 1 ], Integer 2)
            Expect.equal (tryParse "(1 . 2)") expect "(1 . 2)"
            let expect =
                DottedList
                    ([ Integer 1
                       Integer 2 ], Integer 3)
            Expect.equal (tryParse "(1 2 . 3)") expect "(1 2 . 3)"
        }

    [<Tests>]
    let ``parse paren with spaces`` =
        test "parse paren with spaces" {
            let expect =
                List
                    [ Atom "+"
                      Integer 1
                      Integer 2 ]
            Expect.equal (tryParse "(+ 1 2)") expect "(+ 1 2)"
            Expect.equal (tryParse "(+   1     2)") expect "(+   1     2)"
            Expect.equal (tryParse "(+ 1 2 )") expect "(+ 1 2 )"
            Expect.equal (tryParse "( + 1 2)") expect "( + 1 2)"
            Expect.equal (tryParse "( + 1 2 )") expect "( + 1 2 )"
        }
