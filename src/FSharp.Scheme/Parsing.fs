namespace FSharp.Scheme.Core.Parsing

open System

module Parsing =
    open FParsec
    open FSharp.Scheme.Core.Ast
    open Ast

    type Parser<'a> = Parser<'a, unit>

    // 再帰的な文法構造において必要
    let parseExpr, parseExprRef: Parser<LispVal> * ref<Parser<LispVal>> = createParserForwardedToRef()

    let symbol: Parser<char> = anyOf "!#$%&|*+-/:<=>?@^_~"

    let internal nonEscapedChar: Parser<char> = noneOf [ '\\'; '"' ]

    let internal convEsc =
        function
        | 'b' -> '\b'
        | 'f' -> '\f'
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | c -> c

    let internal escapedChar: Parser<char> = pchar '\\' >>. anyOf @"\""/bfnrt" |>> convEsc

    let parseAtom: Parser<LispVal> =
        parse {
            let! first = letter <|> symbol
            let! rest = manyChars (letter <|> digit <|> symbol)
            let atom = sprintf "%c%s" first rest
            return match atom with
                   | "#t" -> Bool true
                   | "#f" -> Bool false
                   | _ -> Atom atom
        }

    let parseString: Parser<LispVal> =
        parse {
            do! skipChar '"'
            let! x = manyChars (nonEscapedChar <|> escapedChar)
            do! skipChar '"'
            return String x
        }

    let numberFormat =
        NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction
        ||| NumberLiteralOptions.AllowExponent

    let parseNumber: Parser<LispVal> =
        parse {
            let x = numberLiteral numberFormat "number"
            let! x = x |>> fun x ->
                         if x.IsInteger then Integer(int x.String) else Float(float x.String)
            return x
        }

    let parseList: Parser<LispVal> =
        parse {
            let! xs = sepBy parseExpr spaces1
            return List xs }

    let parseDottedList: Parser<LispVal> =
        parse {
            let! head = sepEndBy parseExpr spaces
            let! tail = pchar '.' >>. spaces >>. parseExpr
            return DottedList(head, tail) }

    let parseQuoted: Parser<LispVal> =
        parse {
            let! xs = pchar ''' >>. parseExpr
            return List
                       [ Atom "quote"
                         xs ]
        }

    let parseListOrDotted: Parser<LispVal> =
        parse {
            do! pchar '(' >>. spaces
            let! xs = attempt parseList <|> parseDottedList
            do! skipChar ')'
            return xs
        }

    do parseExprRef := choice [ parseListOrDotted; parseQuoted; parseNumber; parseAtom; parseString ]

    let readExpr (input: string): string =
        match run (spaces >>. parseExpr .>> eof) input with
        | Success(res, _, _) -> sprintf "Found Value: %A" res
        | Failure(msg, _, _) -> sprintf "No match: %s" msg

    let parseBy (input: string): LispVal =
        match run (spaces >>. parseExpr .>> eof) input with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "%s" msg
