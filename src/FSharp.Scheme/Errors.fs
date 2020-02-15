namespace FSharp.Scheme.Core

module Errors =
    open FSharp.Scheme.Core.Ast

    exception NumArgsException of expected: int * actual: int * LispVal list

    exception TypeMismatchException of string * LispVal

    exception ParserException of string

    exception BadSpecialFormException of msg: string * form: LispVal

    exception NotFunctionException of msg: string * func: string

    exception UnboundVarException of msg: string * varname: string

    exception DefaultException of string

    [<RequireQualifiedAccess>]
    module LispError =

        let toString ex =
            match ex with
            | UnboundVarException(msg, varname) ->
                sprintf "UnboundVar:\n%s: %s" msg varname
            | BadSpecialFormException(msg, form) ->
                sprintf "BadSpecialFormException:\n%s: %s" msg (LispVal.toString form)
            | NotFunctionException(msg, func) ->
                sprintf "NotFunctionException:\n%s: %s" msg func
            | NumArgsException(expected, actual, found) ->
                sprintf "NumArgsException:\nExpected %d args: Actual: %d, values: %s" expected actual
                    (LispVal.unwordsList found)
            | TypeMismatchException(expected, found) ->
                sprintf "TypeMismatchException:\nexpected %s, found: %s" expected (LispVal.toString found)
            | ParserException(msg) ->
                sprintf "ParserException:\n%s" msg
            | DefaultException(msg) ->
                sprintf "DefaultException:\n%s" msg
            | _ -> raise ex // Unhandle
