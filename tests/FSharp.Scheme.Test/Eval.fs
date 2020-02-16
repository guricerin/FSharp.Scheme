module FSharp.Scheme.Test.Eval

open System
open System.IO
open Expecto
open FParsec
open FSharp.Scheme.Core.Types
open FSharp.Scheme.Core.Ast
open FSharp.Scheme.Core.Parsing
open FSharp.Scheme.Core.Env
open FSharp.Scheme.Core.Primitives
open FSharp.Scheme.Core.Eval

module EvalTest =

    let tryParse (input: string): LispVal =
        match run (spaces >>. parseExpr .>> eof) input with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "%s" msg

    let env = Env.init |> Primitives.load

    let tryPES =
        tryParse
        >> eval env
        >> LispVal.toString

    [<Tests>]
    let ``eval int`` =
        test "eval int" {
            let expect = "3"
            Expect.equal (tryPES "(+ 1 2)") expect "(+ 1 2)"
            let expect = "5"
            Expect.equal (tryPES "(+ 2 (- 4 1))") expect "(+ 2 (- 4 1))"
            let expect = "-1"
            Expect.equal (tryPES "(+ 2 -4 1)") expect "(+ 2 -4 1)"
            let expect = "3"
            Expect.equal (tryPES "(- (+ 4 6 3) 3 5 2)") expect "(- (+ 4 6 3) 3 5 2)"
        }

    [<Tests>]
    let ``eval bool`` =
        test "eval bool" {
            let input = "(< 2 3)"
            let expect = "#t"
            Expect.equal (tryPES input) expect input
            let input = "(> 2 3)"
            let expect = "#f"
            Expect.equal (tryPES input) expect input
            let input = "(>= 3 3)"
            let expect = "#t"
            Expect.equal (tryPES input) expect input
            let input = "(string=? \"test\" \"test\")"
            let expect = "#t"
            Expect.equal (tryPES input) expect input
            let input = "(string<? \"abc\" \"bba\")"
            let expect = "#t"
            Expect.equal (tryPES input) expect input
        }

    [<Tests>]
    let ``eval if`` =
        test "eval if" {
            let input = "(if (> 2 3) \"no\" \"yes\")"
            let expect = "\"yes\""
            Expect.equal (tryPES input) expect input
            let input = "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")"
            let expect = "9"
            Expect.equal (tryPES input) expect input
        }

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

    [<Tests>]
    let ``define`` =
        let envDefine = Env.init |> Primitives.load

        let tryPES =
            tryParse
            >> eval envDefine
            >> LispVal.toString
        test "define" {
            let input = "(define x 3)"
            let expect = "3"
            Expect.equal (tryPES input) expect input
            let input = "(+ x 2)"
            let expect = "5"
            Expect.equal (tryPES input) expect input
            let input = "(define y 5)"
            let expect = "5"
            Expect.equal (tryPES input) expect input
            let input = "(+ x (- y 2))"
            let expect = "6"
            Expect.equal (tryPES input) expect input
            let input = "(define str  \"A string\")"
            let expect = "\"A string\""
            Expect.equal (tryPES input) expect input
            let input = "(string<? str \"The string\")"
            let expect = "#t"
            Expect.equal (tryPES input) expect input
        }

    [<Tests>]
    let ``define func`` =
        let envDefineFunc = Env.init |> Primitives.load

        let tryPES =
            tryParse
            >> eval envDefineFunc
            >> LispVal.toString

        test "define func" {
            let input = "(define (f x y) (+ x y))"
            let expect = "(lambda (x y) ...)"
            Expect.equal (tryPES input) expect input
            let input = "(f 1 2)"
            let expect = "3"
            Expect.equal (tryPES input) expect input
        }

    [<Tests>]
    let ``recurse function`` =
        let envRecurse = Env.init |> Primitives.load

        let tryPES =
            tryParse
            >> eval envRecurse
            >> LispVal.toString

        test "recurse function" {
            let input = "(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))"
            let expect = "(lambda (x) ...)"
            Expect.equal (tryPES input) expect input
            let input = "(factorial 10)"
            let expect = "3628800"
            Expect.equal (tryPES input) expect input
        }

    [<Tests>]
    let ``closure`` =
        let envClosure = Env.init |> Primitives.load

        let tryPES =
            tryParse
            >> eval envClosure
            >> LispVal.toString

        test "closure" {
            let input = "(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))"
            let expect = "(lambda (inc) ...)"
            Expect.equal (tryPES input) expect input
            let input = "(define my-count (counter 5))"
            let expect = "(lambda (x) ...)"
            Expect.equal (tryPES input) expect input
            let input = "(my-count 3)"
            let expect = "8"
            Expect.equal (tryPES input) expect input
            let input = "(my-count 6)"
            let expect = "14"
            Expect.equal (tryPES input) expect input
            let input = "(my-count 5)"
            let expect = "19"
            Expect.equal (tryPES input) expect input
        }

    [<Tests>]
    let ``mutual recursion`` =
        let envMutual = Env.init |> Primitives.load

        let tryPES =
            tryParse
            >> eval envMutual
            >> LispVal.toString

        test "mutual recursion" {
            let input = "(define (f x) (set! y (+ x y)))"
            let expect = "(lambda (x) ...)"
            Expect.equal (tryPES input) expect input
            let input = "(define y 10)"
            let expect = "10"
            Expect.equal (tryPES input) expect input
            let input = "(f 5)"
            let expect = "10"
            Expect.equal (tryPES input) expect input
            let input = "y"
            let expect = "15"
            Expect.equal (tryPES input) expect input
        }

    // [<Tests>]
    let ``std`` =
        let envStd = Env.init |> Primitives.load

        let tryPES =
            tryParse
            >> eval envStd
            >> LispVal.toString

        test "std" {
            let projectPath = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..//..//"))
            let stdfile = @"std.scm"
            let path = Path.Combine(projectPath, stdfile)
            let input = sprintf "(load \"%s\")" path
            let expect = "(lambda (pred xs) ...)"
            Expect.equal (tryPES input) expect input
            let input = "(define xs '(1 2 3 4 5))"
            let expect = "(1 2 3 4 5)"
            Expect.equal (tryPES input) expect input
            let input = "(define ys (map (curry * 2) xs))"
            let expect = "(2 4 6 8 10)"
            Expect.equal (tryPES input) expect input
            let input = "(apply sum (filter (curry < 5) ys))"
            let expect = "24"
            Expect.equal (tryPES input) expect input
        }
