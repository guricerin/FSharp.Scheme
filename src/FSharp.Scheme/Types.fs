namespace FSharp.Scheme.Core

module Types =
    open System.Collections.Generic

    type LispVal =
        | Atom of string
        | List of LispVal list
        | DottedList of LispVal list * LispVal
        | Integer of int
        | Float of float
        | String of string
        | Bool of bool
        | PrimitiveFunc of (LispVal list -> LispVal)
        | Func of Func

    and Func =
        { param: string list
          vararg: string option // 可変長引数
          body: LispVal list
          closure: Env }

    and Env = SortedDictionary<string, LispVal>
