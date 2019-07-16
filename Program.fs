open System
open FSharp.Text.Lexing
open Semantics
open Syntax

let fun3BTy a b c = FunBTy(a, FunBTy(b, c))

let embed : list<ValName * Schema<RType>> =
    [ (ValName "+", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "*", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "/", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "%", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "**", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "negate", Monotype(FunRTy(ZahlRTy, ZahlRTy)))
      (ValName "-", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "!", Monotype(FunRTy(BoolRTy, BoolRTy)))
      (ValName "&&", Monotype(FunRTy(BoolRTy, FunRTy(BoolRTy, BoolRTy))))
      (ValName "||", Monotype(FunRTy(BoolRTy, FunRTy(BoolRTy, BoolRTy))))
      (ValName "count", Polytype(TyName "n", Monotype(FunRTy(FunRTy(VarRTy(TyName "n"), BoolRTy), NatRTy))))
      (ValName "max", Polytype(TyName "n", Monotype(FunRTy(FunRTy(VarRTy(TyName "n"), ZahlRTy), ZahlRTy))))
      (ValName "min", Polytype(TyName "n", Monotype(FunRTy(FunRTy(VarRTy(TyName "n"), ZahlRTy), ZahlRTy)))) ]

let prepare (parsed : Program) =
    let gensym_t = newGensym TyName "_t"
    let gensym_v = newGensym ValName "_v"
    let given = ref (Map.ofList [])
    let definition = ref (Map.ofList [])
    let typeenv = ref (Map.ofList embed)
    for decl in parsed.toplevel do
        match decl with
        | Let(x, [], t, e) ->
            printfn "let %A" x
            let e = convertFromParsedExpr gensym_t gensym_v (!typeenv) e
            let (e, scm) = inferTypes gensym_t e (Option.map convertFromParsedType t)
            typeenv := (!typeenv).Add(x, scm)
            definition := (!definition).Add(x, e)
            printfn "    : %A" scm
        | LetRec(x, t, patterns) ->
            printfn "let rec %A" x
            failwith "\"let rec ...\" is not implemented yet"
        | LetGiven(x, [], t) ->
            printfn "let given %A" x
            let t = convertFromParsedType t
            typeenv := (!typeenv).Add(x, Monotype t)
            printfn "    : %A" t
        | _ -> failwith "params are not implemented yet"
    printfn "in ..."
    let e = convertFromParsedExpr gensym_t gensym_v (!typeenv) parsed.expr
    let (e, scm) = inferTypes gensym_t e None
    printfn "    : %A" scm
    (e, scm)

[<EntryPoint>]
let main argv =
    let lexbuf = LexBuffer<char>.FromTextReader Console.In

    let parsed =
        try
            Parser.start Lexer.tokenstream lexbuf
        with e when e.Message.Equals "parse error" -> raise (Exception(sprintf "SyntaxError: Unexpected token: \"%s\" Line: %d Column: %d" (LexBuffer<_>.LexemeString lexbuf) (lexbuf.StartPos.Line + 1) lexbuf.StartPos.Column))
    printfn "%A" (prepare parsed)
    0
