namespace FSharp.Scheme.Core

module Env =
    open System
    open System.Collections.Generic
    open FSharp.Scheme.Core.Ast
    open FSharp.Scheme.Core.Errors

    type Env = SortedDictionary<string, LispVal>

    [<RequireQualifiedAccess>]
    module Env =

        let init: Env = Env()

        let isBound (env: Env) (varname: string): bool = env.ContainsKey(varname)

        let getVar (env: Env) (varname: string): LispVal =
            match env.TryGetValue(varname) with
            | true, x -> x
            | _ -> raise <| UnboundVarException("Getting an unbound variable", varname)

        let setVar (env: Env) (varname: string) (x: LispVal) =
            match isBound env varname with
            | true ->
                env.[varname] <- x
                x
            | false -> raise <| UnboundVarException("Setting an unbound variable", varname)

        let defineVar (env: Env) (varname: string) (x: LispVal) =
            match isBound env varname with
            | true -> setVar env varname x |> ignore
            | false -> env.Add(varname, x)
            x
