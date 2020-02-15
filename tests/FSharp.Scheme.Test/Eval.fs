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

    [<Tests>]
    let ``eval if`` =
        test "eval if" {
            let input = "(if (> 2 3) \"no\" \"yes\")"
            let expect = String "yes"
            Expect.equal (tryPE input) expect input
            let input = "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")"
            let expect = Integer 9
            Expect.equal (tryPE input) expect input
        }

    let tryPES = tryPE >> LispVal.toString

    [<Tests>]
    let ``eval list operation`` =
        test "eval list operation" {
            let input = "(cdr '(a simple test))"
            let expect = "(simple test)"
            Expect.equal (tryPES input) expect input
            let input = "(car (cdr '(a simple test)))"
            let expect = "simple"
            Expect.equal (tryPES input) expect input
            let input = "(car '((this is) a test))"
            let expect = "(this is)"
            Expect.equal (tryPES input) expect input
            let input = "(cons '(this is) 'test)"
            let expect = "((this is) . test)"
            Expect.equal (tryPES input) expect input
            let input = "(cons '(this is) '())"
            let expect = "((this is))"
            Expect.equal (tryPES input) expect input
            let input = "(eqv? 1 3)"
            let expect = "#f"
            Expect.equal (tryPES input) expect input
            let input = "(eqv? 3 3)"
            let expect = "#t"
            Expect.equal (tryPES input) expect input
            let input = "(eqv? 'atom 'atom)"
            let expect = "#t"
            Expect.equal (tryPES input) expect input
        }
