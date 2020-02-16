namespace FSharp.Scheme.Core

module Env =
    open FSharp.Scheme.Core.Types
    open FSharp.Scheme.Core.Errors

    [<RequireQualifiedAccess>]
    module Env =

        let init: Env = Map.empty |> ref

        let clone (env: Env) =
            seq {
                for kv in !env do
                    (kv.Key, kv.Value)
            }
            |> Map.ofSeq
            |> ref

        let isBound (env: Env) (varname: string): bool = Map.containsKey varname !env

        let getVar (env: Env) (varname: string): LispVal =
            match Map.tryFind varname !env with
            | Some x -> !x
            | _ -> raise <| UnboundVarException("Getting an unbound variable", varname)

        let setVar (env: Env) (varname: string) (x: LispVal) =
            match Map.tryFind varname !env with
            | Some oldx ->
                oldx := x
                x
            | _ -> raise <| UnboundVarException("Setting an unbound variable", varname)

        let defineVar (env: Env) (varname: string) (x: LispVal) =
            env := Map.add varname (ref x) !env
            x

        let bindVars (env: Env) (bindings: (string * LispVal) list) =
            let f (v, e) = env := Map.add v (ref e) !env
            List.iter f bindings
