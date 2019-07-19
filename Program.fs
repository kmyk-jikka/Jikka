open System
open FSharp.Text.Lexing
open Semantics
open Syntax
open Optimization
open Transpile

[<EntryPoint>]
let main argv =
    let lexbuf = LexBuffer<char>.FromTextReader Console.In

    let parsed =
        try
            Parser.start Lexer.tokenstream lexbuf
        with e when e.Message.Equals "parse error" -> raise (Exception(sprintf "SyntaxError: Unexpected token: \"%s\" Line: %d Column: %d" (LexBuffer<_>.LexemeString lexbuf) (lexbuf.StartPos.Line + 1) lexbuf.StartPos.Column))

    let gensym_t = newGensym TyName "_t"
    let gensym_v = newGensym ValName "_v"
    let given = ref []
    let definition = ref []
    let typeenv = ref (Map.ofList embed)
    for decl in parsed.toplevel do
        match decl with
        | Let(x, [], t, e) ->
            eprintfn "let %A" x
            let e = convertFromParsedExpr gensym_t gensym_v (!typeenv) e
            let (e, scm) = inferTypes gensym_t e (Option.map convertFromParsedType t)
            let e = optimize gensym_t e
            typeenv := (!typeenv).Add(x, scm)
            definition := (x, e, scm) :: !definition
            eprintfn "    : %A" scm
        | LetRec(x, t, patterns) ->
            eprintfn "let rec %A" x
            failwith "\"let rec ...\" is not implemented yet"
        | LetGiven(x, [], t) ->
            eprintfn "let given %A" x
            let t = convertFromParsedType t
            typeenv := (!typeenv).Add(x, Monotype t)
            given := (x, t) :: !given
            eprintfn "    : %A" t
        | _ -> failwith "params are not implemented yet"
    eprintfn "in ..."
    let e = convertFromParsedExpr gensym_t gensym_v (!typeenv) parsed.expr
    let (e, scm) = inferTypes gensym_t e None
    eprintfn "%A : %A" e scm
    eprintfn "optimize..."
    let e = optimize gensym_t e
    eprintfn "%A" e
    eprintfn "transpile..."
    let code = transpile !given !definition e scm
    printf "%s" code
    0
