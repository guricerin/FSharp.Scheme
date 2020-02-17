namespace FSharp.Scheme.Core

module Types =
    open System.IO
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
        | PortIn of StreamReader
        | PortOut of StreamWriter

    and Func =
        { parms: string list
          vararg: string option // 可変長引数
          body: LispVal list
          closure: EnvChain }

    and Env = SortedDictionary<string, LispVal>

    and EnvChain = Env list
