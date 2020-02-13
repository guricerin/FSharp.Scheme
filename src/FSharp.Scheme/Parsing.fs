namespace FSharp.Scheme.Core.Parsing

open System

module Parsing =
    open FParsec
    open FSharp.Scheme.Core.Ast

    type Parser<'a> = Parser<'a, unit>

    let symbol: Parser<char> = anyOf "!#$%&|*+-/:<=>?@^_~"

    let myspaces = spaces1

    let nonEscapedChar: Parser<char> = noneOf [ '\\'; '"' ]

    let convEsc =
        function
        | 'b' -> '\b'
        | 'f' -> '\f'
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | c -> c

    let escapedChar: Parser<char> = pchar '\\' >>. anyOf @"\""/bfnrt" |>> convEsc

    let parseAtom: Parser<Ast.LispVal> =
        parse {
            let! first = letter <|> symbol
            let! rest = manyChars (letter <|> digit <|> symbol)
            let atom = sprintf "%c%s" first rest
            return match atom with
                   | "#t" -> Ast.Bool true
                   | "#f" -> Ast.Bool false
                   | _ -> Ast.Atom atom
        }

    let parseString: Parser<Ast.LispVal> =
        parse {
            let! x = manyChars (nonEscapedChar <|> escapedChar) |> between (pchar '"') (pchar '"')
            return Ast.String x }

    let parseCharcter: Parser<Ast.LispVal> =
        parse {
            let! x = (nonEscapedChar <|> escapedChar) |> between (pchar ''') (pchar ''')
            return Ast.Character x }

    let numberFormat =
        NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction
        ||| NumberLiteralOptions.AllowExponent

    let parseNumber: Parser<Ast.LispVal> =
        parse {
            let x = numberLiteral numberFormat "number"
            let! x = x |>> fun x ->
                         if x.IsInteger then Ast.Integer(int x.String) else Ast.Float(float x.String)
            return x
        }

    let parseExpr: Parser<Ast.LispVal> = parseAtom <|> parseString <|> parseCharcter <|> parseNumber

    let readExpr (input: string): string =
        match run parseExpr input with
        | Success(res, _, _) -> sprintf "Found Value: %A" res
        | Failure(msg, _, _) -> sprintf "No match: %s" msg
