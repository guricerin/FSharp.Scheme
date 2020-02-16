namespace FSharp.Scheme.Core

module Env =
    open FSharp.Scheme.Core.Types
    open FSharp.Scheme.Core.Errors

    [<RequireQualifiedAccess>]
    module Env =

        let init: Env = Env()

        let clone (env: Env) = Env(env)

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

        let bindVars (env: Env) (bindings: (string * LispVal) list) = List.iter (fun (k, v) -> env.Add(k, v)) bindings
