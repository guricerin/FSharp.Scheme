namespace FSharp.Scheme.Core

module Eval =
    open FSharp.Scheme.Core.Types
    open FSharp.Scheme.Core.Ast
    open FSharp.Scheme.Core.Env
    open FSharp.Scheme.Core.Errors
    open FSharp.Scheme.Core.Io

    let initFunc varargs env parms body =
        let deatom =
            function
            | Atom a -> a
            | e -> raise <| TypeMismatchException("Function parameter must be Atom", e)
        { parms = List.map LispVal.toString parms
          varargs = varargs
          body = body
          closure = env }
        |> Func

    let initNormalFunc = initFunc None

    let initVarArgs varargs =
        varargs
        |> LispVal.toString
        |> Some
        |> initFunc

    let internal safeZip xs ys =
        let rec loop ls rs acc =
            match ls, rs with
            | [], _ -> acc
            | _, [] -> acc
            | l :: ls, r :: rs -> loop ls rs ((l, r) :: acc)
        loop xs ys [] |> List.rev

    let rec eval (env: Env) (x: LispVal): LispVal =
        match x with
        | String _
        | Integer _
        | Float _
        | Bool _ -> x
        | Atom id -> Env.getVar env id
        | List [ Atom "quote"; x ] -> x // クォート外し
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
        | List(Atom "define" :: List(Atom var :: parms) :: body) ->
            initNormalFunc env parms body |> Env.defineVar env var
        | List(Atom "define" :: DottedList(Atom var :: parms, varargs) :: body) ->
            initVarArgs varargs env parms body |> Env.defineVar env var
        | List(Atom "lambda" :: List parms :: body) -> initNormalFunc env parms body
        | List(Atom "lambda" :: DottedList(parms, varargs) :: body) -> initVarArgs varargs env parms body
        | List(Atom "lambda" :: Atom varargs :: body) -> initVarArgs (Atom varargs) env body []
        | List [ Atom "load"; String filename ] ->
            PortIn.load filename
            |> List.map (eval env)
            |> List.last
        | List [ Atom "apply"; func; args ] ->
            let func = eval env func
            match eval env args with
            | List args -> apply func args
            | arg -> apply func [ arg ]
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
            let nparms, nargs = List.length func.parms, List.length args

            let evalBody env =
                func.body
                |> List.map (eval env)
                |> List.last

            let remainingArgs = List.skip nparms args

            let bindVarArgs arg env =
                match arg with
                | Some argname -> Env.bindVars env [ (argname, List remainingArgs) ]
                | None -> env

            if nparms <> nargs && func.varargs = None then
                raise <| NumArgsException(nparms, nargs, args)
            else
                safeZip func.parms args
                |> Env.bindVars func.closure
                |> bindVarArgs func.varargs
                |> evalBody
        | _ -> raise <| NotFunctionException("Non function", LispVal.toString func)
