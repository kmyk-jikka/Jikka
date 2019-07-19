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
        | Let(x, [], t, e) ->
            eprintfn "let %A" x
            let e = convertFromParsedExpr gensym_t gensym_v (getTypeEnv !toplevel) e
            let (e, scm) = inferTypes gensym_t e (Option.map convertFromParsedType t)
            let e = optimize gensym_t e
            toplevel := Defined(x, e, scm) :: !toplevel
            eprintfn "    : %A" scm
        | LetRec(x, t, patterns) ->
            eprintfn "let rec %A" x
            failwith "\"let rec ...\" is not implemented yet"
        | LetGiven(x, [], t) ->
            eprintfn "let given %A" x
            let t = convertFromParsedType t
            toplevel := Given(x, t) :: !toplevel
            eprintfn "    : %A" t
        | _ -> failwith "params are not implemented yet"
    eprintfn "in ..."
    let e = convertFromParsedExpr gensym_t gensym_v (getTypeEnv !toplevel) parsed.expr
    let (e, scm) = inferTypes gensym_t e None
    eprintfn "%A : %A" e scm
    eprintfn "optimize..."
    let e = optimize gensym_t e
    eprintfn "%A" e
    eprintfn "transpile..."
    let code = transpile !toplevel e scm
    printf "%s" code
    0
