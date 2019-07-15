open System
open FSharp.Text.Lexing
open Semantics
open Syntax

let embed : list<ValName * Schema<BType>> =
    [ (ValName "+", Monotype(FunBTy(ZahlBTy, FunBTy(ZahlBTy, ZahlBTy))))
      (ValName "*", Monotype(FunBTy(ZahlBTy, FunBTy(ZahlBTy, ZahlBTy))))
      (ValName "/", Monotype(FunBTy(ZahlBTy, FunBTy(ZahlBTy, ZahlBTy))))
      (ValName "%", Monotype(FunBTy(ZahlBTy, FunBTy(ZahlBTy, ZahlBTy))))
      (ValName "**", Monotype(FunBTy(ZahlBTy, FunBTy(ZahlBTy, ZahlBTy))))
      (ValName "negate", Monotype(FunBTy(ZahlBTy, ZahlBTy)))
      (ValName "-", Monotype(FunBTy(ZahlBTy, FunBTy(ZahlBTy, ZahlBTy))))
      (ValName "!", Monotype(FunBTy(BoolBTy, BoolBTy)))
      (ValName "&&", Monotype(FunBTy(BoolBTy, FunBTy(BoolBTy, BoolBTy))))
      (ValName "||", Monotype(FunBTy(BoolBTy, FunBTy(BoolBTy, BoolBTy))))
      (ValName "count", Polytype(TyName "n", Monotype(FunBTy(FunBTy(VarBTy(TyName "n"), BoolBTy), NatBTy))))
      (ValName "max", Polytype(TyName "n", Monotype(FunBTy(FunBTy(VarBTy(TyName "n"), ZahlBTy), ZahlBTy))))
      (ValName "min", Polytype(TyName "n", Monotype(FunBTy(FunBTy(VarBTy(TyName "n"), ZahlBTy), ZahlBTy)))) ]

let prepare (parsed : Program) =
    let gensym_t = newGensym TyName "_t"
    let gensym_v = newGensym ValName "_v"
    let given = ref (Map.ofList [])
    let definition = ref (Map.ofList [])
    let typeenv = ref (Map.ofList embed)
    for decl in parsed.toplevel do
        match decl with
        | Let(x, [], t, e) ->
            printfn "let %A ... = ..." x
            let e = convertFromParsedExpr gensym_t gensym_v (!typeenv) e
            let (e, scm) = inferTypes gensym_t e (Option.map convertFromParsedType t)
            typeenv := (!typeenv).Add(x, scm)
            definition := (!definition).Add(x, e)
        | LetRec(x, t, patterns) ->
            printfn "let rec %A ... = ..." x
            failwith "\"let rec ...\" is not implemented yet"
        | LetGiven(x, [], t) ->
            printfn "let given %A ... = ..." x
            let t = convertFromParsedType t
            typeenv := (!typeenv).Add(x, Monotype t)
        | _ -> failwith "params are not implemented yet"
    printfn "in ..."
    let e = convertFromParsedExpr gensym_t gensym_v (!typeenv) parsed.expr
    let (e, scm) = inferTypes gensym_t e None
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
