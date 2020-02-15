module FSharp.Scheme.Test.Eval

open System
open Expecto
open FParsec
open FSharp.Scheme.Core.Ast
open FSharp.Scheme.Core.Parsing
open FSharp.Scheme.Core.Eval

module EvalTest =

    let tryParse (input: string): LispVal =
        match run (spaces >>. parseExpr .>> eof) input with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "%s" msg

    let tryPE = tryParse >> eval

    [<Tests>]
    let ``eval int`` =
        test "eval int" {
            let expect = Integer 3
            Expect.equal (tryPE "(+ 1 2)") expect "(+ 1 2)"
            let expect = Integer 5
            Expect.equal (tryPE "(+ 2 (- 4 1))") expect "(+ 2 (- 4 1))"
            let expect = Integer -1
            Expect.equal (tryPE "(+ 2 -4 1)") expect "(+ 2 -4 1)"
            let expect = Integer 3
            Expect.equal (tryPE "(- (+ 4 6 3) 3 5 2)") expect "(- (+ 4 6 3) 3 5 2)"
        }

    [<Tests>]
    let ``eval bool`` =
        test "eval bool" {
            let input = "(< 2 3)"
            let expect = Bool true
            Expect.equal (tryPE input) expect input
            let input = "(> 2 3)"
            let expect = Bool false
            Expect.equal (tryPE input) expect input
            let input = "(>= 3 3)"
            let expect = Bool true
            Expect.equal (tryPE input) expect input
            let input = "(string=? \"test\" \"test\")"
            let expect = Bool true
            Expect.equal (tryPE input) expect input
            let input = "(string<? \"abc\" \"bba\")"
            let expect = Bool true
            Expect.equal (tryPE input) expect input
        }
