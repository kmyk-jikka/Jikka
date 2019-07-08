open System
open FSharp.Text.Lexing
open Semantics

[<EntryPoint>]
let main argv =
    // let lexbuf = LexBuffer<char>.FromTextReader Console.In
    // let parsed =
    //     try
    //         Parser.start Lexer.tokenstream lexbuf
    //     with
    //         | e when e.Message.Equals "parse error" ->
    //             raise (Exception (sprintf "SyntaxError: Unexpected token: \"%s\" Line: %d Column: %d" (LexBuffer<_>.LexemeString lexbuf) (lexbuf.StartPos.Line + 1) lexbuf.StartPos.Column))
    // printfn "%A" parsed

    let a = Ident "A"
    let b = Ident "B"
    let plus = Ident "+"
    let plusType = FunBTy (IntBTy, FunBTy (IntBTy, IntBTy))
    let f = Ident "f"
    let x = Ident "x"
    let y = Ident "y"
    // let term = LamUExp (x, LamAsUExp (y, a, AppUExp (AppUExp (VarUExp plus, AsUExp (VarUExp x, IntBTy)), VarUExp y)))  // \ x. \ (y : A). (x : Nat) + y
    let scm = LamBScm (a, BaseBScm (FunBTy (FunBTy (VarBTy a, VarBTy a), FunBTy (VarBTy a, VarBTy a))))  // forall A. (A -> A) -> A -> A
    // let scm = LamBScm (a, LamBScm (b, BaseBScm (FunBTy (FunBTy (VarBTy a, VarBTy a), FunBTy (VarBTy b, VarBTy b)))))  // forall A. forall B. (A -> A) -> B -> B
    let term = LamUExp (f, LamUExp (x, AppUExp (VarUExp f, AppUExp (VarUExp f, VarUExp x))))  // \ f. \ x. f (f x)
    // let term = LamUExp (f, AppUExp (VarUExp f, VarUExp f))  // \ f. f f
    printfn "term: %A" term
    printfn "schema: %A" scm
    printfn "result: %A" (inferTypes term (Some scm))

    0
