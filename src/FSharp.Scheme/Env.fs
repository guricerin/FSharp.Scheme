namespace FSharp.Scheme.Core

module Env =
    open FSharp.Scheme.Core.Types
    open FSharp.Scheme.Core.Errors

    [<RequireQualifiedAccess>]
    module Env =

        let init: Env = ref []

        /// xは変更されない
        let newRef (x: 'a) = ref x

        let isBound env varname = List.exists (fun (k, v) -> k = varname) !env

        let getVar (env: Env) (varname: string): LispVal =
            match List.tryFind (fun (k, v) -> k = varname) !env with
            | Some(_, x) -> !x
            | None -> raise <| UnboundVarException("Getting an unbound variable", varname)

        let setVar (env: Env) (varname: string) (x: LispVal) =
            match List.tryFind (fun (k, v) -> k = varname) !env with
            | Some(_, oldx) ->
                oldx := x
                x
            | None -> raise <| UnboundVarException("Setting an unbound variable", varname)

        let defineVar (env: Env) (varname: string) (x: LispVal) =
            if isBound env varname then
                setVar env varname x
            else
                let value = newRef x
                env := (varname, value) :: !env
                x

        let bindVars (env: Env) (bindings: (string * LispVal) list): Env =
            let addBinding (varname, value) = (varname, newRef value)

            let extendEnv =
                bindings
                |> List.map addBinding
                |> List.fold (fun acc a -> a :: acc) !env
            newRef extendEnv
