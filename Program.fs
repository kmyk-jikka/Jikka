open System
open System.Text
open FSharp.Text.Lexing
open Semantics
open Syntax
open Optimization

let rec getCXXType : RType -> string =
    function
    | VarRTy _ as t -> failwithf "failed to convert to a C++ type: %A" t
    | FunRTy(ZahlRTy, t) -> sprintf "vector<%s>" (getCXXType t)
    | FunRTy(NatRTy, t) -> sprintf "vector<%s>" (getCXXType t)
    | FunRTy(OrdinalRTy _, t) -> sprintf "vector<%s>" (getCXXType t)
    | FunRTy(RangeRTy _, t) -> sprintf "vector<%s>" (getCXXType t)
    | FunRTy _ as t -> failwithf "failed to convert to a C++ type: %A" t
    | ZahlRTy -> "int64_t"
    | NatRTy -> "int64_t"
    | OrdinalRTy _ -> "int64_t"
    | RangeRTy _ -> "int64_t"
    | BoolRTy -> "bool"

let getCXXArgumentType : RType -> string =
    function
    | FunRTy(ZahlRTy, t) -> sprintf "const vector<%s> &" (getCXXType t)
    | FunRTy(NatRTy, t) -> sprintf "const vector<%s> &" (getCXXType t)
    | FunRTy(OrdinalRTy _, t) -> sprintf "const vector<%s> &" (getCXXType t)
    | FunRTy(RangeRTy _, t) -> sprintf "const vector<%s> &" (getCXXType t)
    | t -> getCXXType t

let getCXXTypeFromSchema : Schema<RType> -> string =
    function
    | Polytype _ as scm -> failwithf "failed to convert to a C++ type: %A" scm
    | Monotype t -> getCXXType t

let indent (nest : int) : string = String.replicate nest "    "

let join (sep : string) (words : list<string>) : string =
    List.fold (fun acc (i, word) ->
        acc + (if i = 0 then ""
               else sep)
        + word) "" (List.indexed words)

let transpileExpr (gensym : unit -> string) (sb : StringBuilder) : int -> Expr -> StringBuilder =
    let appendLine (nest : int) (s : string) =
        let _ = sb.Append(indent nest)
        let _ = sb.Append s

        let _ =
            sb.Append "\n"
        ()

    let rec go (env : list<string>) (nest : int) : Expr -> unit =
        let accumulate plus zero n e =
            let acc = gensym()
            let cnt = gensym()
            appendLine nest (sprintf "int64_t %s = %s;" acc zero)
            appendLine nest (sprintf "int64_t %s = ([&]() {" cnt)
            go env (nest + 1) n
            appendLine nest "})();"
            appendLine nest (sprintf "while (%s --) {" cnt)
            let t = gensym()
            appendLine (nest + 1) (sprintf "int64_t %s = ([&]() {" t)
            go (cnt :: env) (nest + 2) e
            appendLine (nest + 1) "})();"
            appendLine (nest + 1) (sprintf "%s = %s;" acc (plus acc t))
            appendLine nest "}"
            appendLine nest (sprintf "return %s;" acc)

        let cast t e =
            appendLine nest (sprintf "return ([&]() -> %s {" (getCXXType t))
            go env (nest + 1) e
            appendLine nest "})();"

        function
        | AppExp(AppExp(FreeVarExp(ValName "count", _), n), LamExp(_, e)) -> accumulate (sprintf "%s + (bool)(%s)") "0" n e
        | AppExp(AppExp(FreeVarExp(ValName "sum", _), n), LamExp(_, e)) -> accumulate (sprintf "%s + %s") "0" n e
        | AppExp(AppExp(FreeVarExp(ValName "max", _), n), LamExp(_, e)) -> accumulate (sprintf "max(%s, %s)") "INT64_MIN" n e
        | AppExp(AppExp(FreeVarExp(ValName "min", _), n), LamExp(_, e)) -> accumulate (sprintf "min(%s, %s)") "INT64_MAX" n e
        | AppExp(FreeVarExp(ValName "zahlToBool", _), e) -> cast BoolRTy e
        | AppExp(FreeVarExp(ValName "boolToZahl", _), e) -> cast ZahlRTy e
        | VarExp i -> appendLine nest (sprintf "return %s;" env.[i])
        | FreeVarExp(ValName x, _) -> appendLine nest (sprintf "return %s;" x)
        | LamExp _ as e -> failwithf "failed to transpile: %A" e
        | AppExp _ as e ->
            let rec loop args =
                function
                | AppExp(e1, e2) ->
                    let t = gensym()
                    appendLine nest (sprintf "auto %s = ([&]() {" t)
                    go env (nest + 1) e2
                    appendLine nest "})();"
                    loop (t :: args) e1
                | FreeVarExp(ValName name, _) ->
                    let s =
                        match name with
                        | _ when name = "+" || name = "-" || name = "*" || name = "/" || name = "%" -> join (" " + name + " ") args
                        | _ when name = "**" -> sprintf "pow(%s)" (join ", " args)
                        | _ -> sprintf "%s[%s]" name (join "][" args)
                    appendLine nest (sprintf "return %s;" s)
                | _ -> failwith "failed to transpile"
            loop [] e
        | IfThenElseExp(e1, e2, e3) ->
            let t = gensym()
            appendLine nest (sprintf "bool %s = ([&]() {" t)
            appendLine nest "})();"
            appendLine nest (sprintf "if (%s) {" t)
            go env (nest + 1) e2
            appendLine nest (sprintf "} else {")
            go env (nest + 1) e3
            appendLine nest (sprintf "}")
        | IntExp n -> appendLine nest (sprintf "return %A;" n)
        | BoolExp p ->
            appendLine nest (sprintf "return %s;" (if p then "true"
                                                   else "false"))

    fun nest e ->
        go [] nest e
        sb

let transpileDefinition (gensym : unit -> string) (sb : StringBuilder) (nest : int) (name : ValName) (e : Expr) (scm : Schema<RType>) : StringBuilder =
    let _ = sb.Append(indent nest)
    let _ = sb.Append(getCXXTypeFromSchema scm)
    let _ = sb.Append " "
    match name with
    | ValName name ->
        let _ = sb.Append name
        ()
    let _ =
        sb.Append " = ([&]() {\n"
    match scm with
    | Polytype _ as scm -> failwithf "failed to transpile: %A" scm
    | Monotype t ->
        let _ = transpileExpr gensym sb (nest + 1) e
        ()
    let _ = sb.Append(indent nest)

    let _ =
        sb.Append "})();\n"
    sb

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
    let sb = new StringBuilder()
    let gensym_cxx = newGensym id "t"
    let _ = sb.Append(getCXXTypeFromSchema scm)
    let _ = sb.Append " solve("
    for (i, (ValName x, t)) in List.indexed (List.rev !given) do
        let _ =
            sb.Append(if i = 0 then ""
                      else ", ")

        let _ = sb.Append(getCXXArgumentType t)
        let _ = sb.Append " "
        let _ = sb.Append x
        ()
    let _ =
        sb.Append ") {\n"
    for (x, e, scm) in List.rev !definition do
        let _ = transpileDefinition gensym_cxx sb 1 x e scm
        ()
    let _ = transpileExpr gensym_cxx sb 1 e

    let _ =
        sb.Append "}\n"

    let code = sb.ToString()
    printf "%s" code
    0
