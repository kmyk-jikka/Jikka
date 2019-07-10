open System
open FSharp.Text.Lexing
open Semantics
open Syntax

let embed : list<Ident * BSchema> =
    [ (Ident "+", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "-", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "*", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "/", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "%", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "**", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "max", BaseBScm(FunBTy(IntBTy, FunBTy(FunBTy(IntBTy, IntBTy), IntBTy))))
      (Ident "min", BaseBScm(FunBTy(IntBTy, FunBTy(FunBTy(IntBTy, IntBTy), IntBTy))))
      (Ident "sum", BaseBScm(FunBTy(IntBTy, FunBTy(FunBTy(IntBTy, IntBTy), IntBTy))))
      (Ident "count", BaseBScm(FunBTy(IntBTy, FunBTy(FunBTy(IntBTy, IntBTy), IntBTy))))
      (Ident "<", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "<=", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "==", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "/=", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident ">=", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident ">", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "not", BaseBScm(FunBTy(IntBTy, IntBTy)))
      (Ident "and", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "or", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "implies", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "iff", BaseBScm(FunBTy(IntBTy, FunBTy(IntBTy, IntBTy))))
      (Ident "forall", BaseBScm(FunBTy(FunBTy(IntBTy, IntBTy), IntBTy)))
      (Ident "exists", BaseBScm(FunBTy(FunBTy(IntBTy, IntBTy), IntBTy))) ]

type Problem =
    { compiletime : list<Ident * BExpr * BSchema>
      input : list<Ident * BType>
      runtime : list<Ident * BExpr * BType>
      assumptions : list<BExpr>
      output : BExpr * BType }

let prepare (program : Program) : Problem =
    let compiletime = ref []
    let compiletimeTypeEnv = ref (Map.ofList embed)
    let input = ref []
    let runtime = ref []
    let runtimeTypeEnv = ref (Map.ofList embed)
    let assumptions = ref []
    let decl = ref None
    let body = ref []

    let flush (() : unit) : unit =
        match !decl with
        | None -> ()
        | Some(name, _) when (!runtimeTypeEnv).ContainsKey name -> failwithf "symbol already defined: %A" name
        | Some(name, annot) ->
            match !body with
            | [] -> failwith "a declaration without definitions found"
            | (args, f) :: [] ->
                printfn "define %A %A : %A = %A" name args annot f
                let gensym = freshIdentGen "_x"

                let push x f =
                    match x with
                    | None -> LamUExp(gensym(), None, f)
                    | Some x -> LamUExp(x, None, f)

                let f = List.foldBack push args f
                if List.forall (!compiletimeTypeEnv).ContainsKey (listFreeVars f) then
                    let (f, scm) = inferTypes (!compiletimeTypeEnv) f annot
                    compiletime := (name, f, scm) :: !compiletime
                    compiletimeTypeEnv := (!compiletimeTypeEnv).Add(name, scm)
                    runtimeTypeEnv := (!runtimeTypeEnv).Add(name, scm)
                else
                    let (f, scm) = inferTypes (!runtimeTypeEnv) f annot
                    match scm with
                    | BaseBScm t ->
                        runtime := (name, f, t) :: !runtime
                        runtimeTypeEnv := (!runtimeTypeEnv).Add(name, BaseBScm t)
                    | _ -> failwith "the type of runtime value should not be a schema"
            | _ :: _ :: _ -> failwith "not implemented yet"
        decl := None
        body := []
    for assumption in program.environment do
        match assumption with
        | Declare(name, scm) ->
            flush()
            decl := Some(name, Some scm)
        | Define(name, args, f) ->
            match !decl with
            | None -> decl := Some(name, None)
            | Some(name_, _) ->
                if name_ = name then ()
                else
                    flush()
                    decl := Some(name, None)
            body := (args, f) :: !body
        | Input(name, t) ->
            printfn "input %A : %A" name t
            input := (name, t) :: !input
            runtimeTypeEnv := (!runtimeTypeEnv).Add(name, BaseBScm t)
        | Assume f ->
            printfn "assume %A" f
            let (f, scm) = inferTypes (!runtimeTypeEnv) f (Some(BaseBScm IntBTy))
            match scm with
            | BaseBScm IntBTy -> assumptions := f :: !assumptions
            | _ -> failwithf "the type of assumption should not be a boolean, but: %A" scm
    flush()
    printfn "output %A" program.output
    let output =
        match inferTypes (!runtimeTypeEnv) program.output None with
        | (f, BaseBScm t) -> (f, t)
        | _ -> failwith "the type of output should not be a schema"
    { compiletime = !compiletime
      input = !input
      runtime = !runtime
      assumptions = !assumptions
      output = output }

[<EntryPoint>]
let main argv =
    let lexbuf = LexBuffer<char>.FromTextReader Console.In

    let parsed =
        try
            Parser.start Lexer.tokenstream lexbuf
        with e when e.Message.Equals "parse error" -> raise (Exception(sprintf "SyntaxError: Unexpected token: \"%s\" Line: %d Column: %d" (LexBuffer<_>.LexemeString lexbuf) (lexbuf.StartPos.Line + 1) lexbuf.StartPos.Column))
    printfn "%A" (prepare parsed)
    0
