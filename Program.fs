module Jikka.Program

open FSharp.Text.Lexing
open Jikka.Optimization
open Jikka.Semantics
open Jikka.Syntax
open Jikka.Transpile
open System

[<EntryPoint>]
let main argv =
    let lexbuf = LexBuffer<char>.FromTextReader Console.In

    let parsed =
        try
            Parser.start Lexer.tokenstream lexbuf
        with e when e.Message.Equals "parse error" -> raise (Exception(sprintf "SyntaxError: Unexpected token: \"%s\" Line: %d Column: %d" (LexBuffer<_>.LexemeString lexbuf) (lexbuf.StartPos.Line + 1) lexbuf.StartPos.Column))

    let gensym_t = newGensym TyName "_t"
    let gensym_v = newGensym ValName "_v"
    let toplevel = ref (List.map Builtin embed)
    for decl in parsed.toplevel do
        match decl with
        | Let(x, args, t, e) ->
            eprintfn "let %A" x
            let e = List.foldBack lambdaFromParam args e
            let e = convertFromParsedExpr gensym_t gensym_v (getTypeEnv !toplevel) [] e
            let t = Option.map (fun t -> List.foldBack (fun _ t2 -> FunRTy(VarRTy(gensym_t()), t2)) args (convertFromParsedType t)) t
            let (e, scm) = inferTypes gensym_t e t
            let e = optimize (getTypeEnv !toplevel) gensym_t e
            let (e, scm) = inferTypes gensym_t e None
            toplevel := Defined(x, e, scm) :: !toplevel
            eprintfn "    : %A" scm
        | LetRec(x, t, patterns) ->
            eprintfn "let rec %A" x
            let (e, t) = convertFromParsedPatterns gensym_t gensym_v (getTypeEnv !toplevel) x t patterns
            let (e, scm) = inferTypes gensym_t e (Some t)
            let e = optimize (getTypeEnv !toplevel) gensym_t e
            let (e, scm) = inferTypes gensym_t e None
            toplevel := Defined(x, e, scm) :: !toplevel
        | LetGiven(x, t) ->
            eprintfn "let given %A" x
            let t = convertFromParsedType t
            toplevel := Given(x, t) :: !toplevel
            eprintfn "    : %A" t
    eprintfn "in ..."
    let e = convertFromParsedExpr gensym_t gensym_v (getTypeEnv !toplevel) [] parsed.expr
    let (e, scm) = inferTypes gensym_t e None
    eprintfn "%A : %A" e scm
    eprintfn "optimize..."
    let e = optimize (getTypeEnv !toplevel) gensym_t e
    let (e, scm) = inferTypes gensym_t e None
    eprintfn "%A" e
    eprintfn "transpile..."
    let code = transpile !toplevel e scm
    printf "%s" code
    0
