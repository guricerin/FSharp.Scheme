namespace FSharp.Scheme.Core

module Env =
    open FSharp.Scheme.Core.Types
    open FSharp.Scheme.Core.Errors

    [<RequireQualifiedAccess>]
    module Env =

        let init: Env = Env()

        let clone (env: Env) = Env(env)

        let tryFind (env: Env) (varname: string): LispVal option =
            match env.TryGetValue varname with
            | true, x -> Some x
            | _ -> None

        let bindVars (env: Env) (bindings: (string * LispVal) list) =
            let f (v, e) = env := Map.add v (ref e) !env
            List.iter f bindings

    [<RequireQualifiedAccess>]
    module EnvChain =

        let init: EnvChain = [ Env.init ]

        let getVar (envchain: EnvChain) (varname: string): LispVal =
            let rec loop ls =
                match ls with
                | [] -> raise <| UnboundVarException("Getting an unbound variable", varname)
                | e :: es ->
                    match Env.tryFind e varname with
                    | Some v -> v
                    | None -> loop es
            loop envchain

        let setVar (envchain: EnvChain) (varname: string) (x: LispVal) =
            match envchain with
            | e :: _ -> e.[varname] <- x
            | [] -> raise <| UnboundVarException("Setting an unbound variable", varname)

        let defineVar (envchain: EnvChain) (varname: string) (x: LispVal) =
            match envchain with
            | e :: _ ->
                match Env.tryFind e varname with
                | Some _ -> e.[varname] <- x
                | None -> e.Add(varname, x)
            | [] -> raise <| DefaultException("Environment is null")
