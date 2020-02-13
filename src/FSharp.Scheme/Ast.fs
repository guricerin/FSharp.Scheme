namespace FSharp.Scheme.Core.Ast

module Ast =

    type LispVal =
        | Atom of string
        | List of LispVal list
        | DottedList of LispVal list * LispVal
        | Integer of int
        | Float of float
        | String of string
        | Character of char
        | Bool of bool
