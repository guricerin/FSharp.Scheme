module FSharp.Scheme.Test.Parsing

open System
open Expecto
open FParsec
open FSharp.Scheme.Core.Types
open FSharp.Scheme.Core.Ast
open FSharp.Scheme.Core.Parsing

module ParsingTest =

    let config = { FsCheckConfig.defaultConfig with maxTest = 100 }

    [<Tests>]
    let ``parse single integer`` =
        let parseTest input expect =
            let actual =
                parseBy input
                |> function
                | Integer n -> n
                | _ -> failwithf "unreachable!"
            actual = expect

        let f (a: int) =
            let s = string a
            let expect = a
            parseTest s expect

        testPropertyWithConfig config "parse single integer" <| f

    // [<Tests>]
    let ``parse single float`` =
        let parseTest input expect =
            let actual =
                parseBy input
                |> function
                | Float f -> f
                | _ -> failwithf "unreachable!"
            actual = expect

        let f (a: float) =
            let s =
                match a with
                | _ when a = infinity -> "Infinity"
                | _ when a = nan -> "NaN"
                | _ when a = Double.NegativeInfinity -> "-Infinity"
                | _ -> string a

            let expect = a

            parseTest s expect

        testPropertyWithConfig config "parse single float" <| f

    let tryParse input = parseBy input |> LispVal.toString

    [<Tests>]
    let ``parse quote symbol`` =
        test "parse quote symbol" {
            Expect.equal (tryParse "quote") "quote" "quote"
            let expect = "(quote 1 2 3)"
            Expect.equal (tryParse "(quote 1 2 3)") expect "(quote 1 2 3)"
            let expect = "(quote (1 2 3))"
            Expect.equal (tryParse "'(1 2 3)") expect "'(1 2 3)"
        }

    [<Tests>]
    let ``parse nested list`` =
        test "parse nested list" {
            let expect = "(a test)"
            Expect.equal (tryParse "(a test)") expect "(a test)"
            let expect = "(a (nested) test)"
            Expect.equal (tryParse "(a (nested) test)") expect "(a (nested) test)"
            let expect = "(a (dotted . list) test)"
            Expect.equal (tryParse "(a (dotted . list) test)") expect "(a (dotted . list) test)"
            let expect = "(a (quote (quoted (dotted . list))) test)"
            Expect.equal (tryParse "(a '(quoted (dotted . list)) test)") expect "(a '(quoted (dotted . list)) test)"
        }

    [<Tests>]
    let ``parse dotted list`` =
        test "parse dotted list" {
            let expect = "(1 . 2)"
            Expect.equal (tryParse "(1 . 2)") expect "(1 . 2)"
            let expect = "(1 2 . 3)"
            Expect.equal (tryParse "(1 2 . 3)") expect "(1 2 . 3)"
        }

    [<Tests>]
    let ``parse paren with spaces`` =
        test "parse paren with spaces" {
            let expect = "(+ 1 2)"
            Expect.equal (tryParse "(+ 1 2)") expect "(+ 1 2)"
            Expect.equal (tryParse "(+   1     2)") expect "(+   1     2)"
            Expect.equal (tryParse "( + 1 2)") expect "( + 1 2)"
            Expect.equal (tryParse "(+ 1 2 )") expect "(+ 1 2 )"
            Expect.equal (tryParse "( + 1 2 )") expect "( + 1 2 )"
        }
