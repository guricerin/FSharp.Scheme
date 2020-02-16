namespace FSharp.Scheme.Core

module Eval =
    open System
    open FSharp.Scheme.Core.Types
    open FSharp.Scheme.Core.Ast
    open FSharp.Scheme.Core.Env
    open FSharp.Scheme.Core.Errors
    open FSharp.Scheme.Core.Io

    let rec bindArgsCore param args (env: Env) =
        match param, args with
        | [], [] -> env
        | p :: ps, a :: ass ->
            Env.defineVar env p a |> ignore
            bindArgsCore ps ass env
        | _ -> raise <| DefaultException("Interpreter error - failed to detect NumArg mismatch")

    let rec bindVarargs param args vararg (env: Env) =
        match param, args with
        | [], [] -> env
        | p :: ps, a :: ass ->
            Env.defineVar env p a |> ignore
            bindVarargs ps ass vararg env
        | [], _ ->
            Env.defineVar env vararg (List args) |> ignore
            env
        | _ -> raise <| DefaultException("Interpreter error - failed to detect NumArg mismatch")

    let bindArgs (func: Func) args =
        let nparam, nargs = List.length func.param, List.length args
        match nparam <> nargs, func.vararg with
        | true, _ -> raise <| NumArgsException(nparam, nargs, args)
        | false, Some varg -> bindVarargs func.param args varg (Env.clone func.closure)
        | false, None -> bindArgsCore func.param args (Env.clone func.closure)

    let initFunc param vararg body env =
        let deatom =
            function
            | Atom a -> a
            | e -> raise <| TypeMismatchException("Function parameter must be Atom", e)
        { param = List.map deatom param
          vararg = vararg
          body = body
          closure = env }
        |> Func

    let rec eval (env: Env) (x: LispVal): LispVal =
        match x with
        | String _
        | Integer _
        | Float _
        | Bool _ -> x
        | Atom id -> Env.getVar env id
        | List [ Atom "quote"; ls ] -> ls // クォート外し
        | List [ Atom "if"; pred; conseq; alt ] ->
            match eval env pred with
            | Bool true -> eval env conseq
            | Bool false -> eval env alt
            | _ -> raise <| BadSpecialFormException("if form should take boolean prediction", x)
        | List [ Atom "set!"; Atom var; form ] ->
            form
            |> eval env
            |> Env.setVar env var
        | List [ Atom "define"; Atom var; form ] ->
            form
            |> eval env
            |> Env.defineVar env var
        | List(Atom "define" :: List(Atom var :: param) :: body) ->
            initFunc param None body env |> Env.defineVar env var
        | List(Atom "define" :: DottedList(Atom var :: param, Atom varg) :: body) ->
            initFunc param (Some varg) body env |> Env.defineVar env var
        | List(Atom "lambda" :: List param :: body) -> initFunc param None body env
        | List(Atom "lambda" :: DottedList(param, Atom varg) :: body) -> initFunc param (Some varg) body env
        | List(Atom "lambda" :: Atom varg :: body) -> initFunc [] (Some varg) body env
        | List [ Atom "load"; String filename ] ->
            PortIn.load filename
            |> List.map (eval env)
            |> List.last
        | List [ Atom "apply"; func; target ] ->
            let func = eval env func
            match eval env target with
            | List args -> apply func args
            | _ -> apply func [ target ]
        | List(Atom "apply" :: func :: args)
        | List(func :: args) ->
            let func = eval env func
            let args = List.map (eval env) args
            apply func args
        | _ -> raise <| BadSpecialFormException("Unrecognized special form", x)

    and apply (func: LispVal) (args: LispVal list): LispVal =
        match func with
        | PrimitiveFunc pfn -> pfn args
        | Func func ->
            let f = bindArgs func args |> eval
            func.body
            |> List.map f
            |> List.last
        | _ -> raise <| NotFunctionException("Non function", LispVal.toString func)
