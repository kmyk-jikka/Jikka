module Transpile

open System.Text
open Semantics

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

let join (sep : string) (words : list<string>) : string =
    List.fold (fun acc (i, word) ->
        acc + (if i = 0 then ""
               else sep)
        + word) "" (List.indexed words)

type CXXCode =
    { sentences : list<string>
      expr : string }

let cxxExpr (s : string) : CXXCode =
    { sentences = []
      expr = s }

let bindCXXCode (code : CXXCode) (sentences : list<string>) (expr : string -> string) =
    { sentences = List.append sentences code.sentences
      expr = expr code.expr }

let transpileExpr (gensym_counter : unit -> string) (gensym_accumulator : unit -> string) (gensym_general : unit -> string) : Expr -> CXXCode =
    let rec go (env : list<string>) : Expr -> CXXCode =
        let accumulate plus zero n e =
            let sentences = ref []
            let line s = sentences := s :: !sentences
            let acc = gensym_accumulator()
            line (sprintf "int64_t %s = %s;" acc zero)
            let n = go env n
            sentences := List.append n.sentences !sentences
            let i = gensym_counter()
            line (sprintf "for (int64_t %s = 0; %s < %s; ++ %s) {" i i n.expr i)
            let e = go (i :: env) e
            sentences := List.append e.sentences !sentences
            line (plus acc e.expr + ";")
            line "}"
            { sentences = !sentences
              expr = acc }
        function
        | AppExp(AppExp(FreeVarExp(ValName "count", _), n), LamExp(_, e)) -> accumulate (sprintf "%s += (bool)(%s)") "0" n e
        | AppExp(AppExp(FreeVarExp(ValName "sum", _), n), LamExp(_, e)) -> accumulate (sprintf "%s += %s") "0" n e
        | AppExp(AppExp(FreeVarExp(ValName "max", _), n), LamExp(_, e)) -> accumulate (fun a b -> sprintf "%s = max(%s, %s)" a a b) "INT64_MIN" n e
        | AppExp(AppExp(FreeVarExp(ValName "min", _), n), LamExp(_, e)) -> accumulate (fun a b -> sprintf "%s = min(%s, %s)" a a b) "INT64_MAX" n e
        | AppExp(FreeVarExp(ValName "zahlToBool", _), e) -> bindCXXCode (go env e) [] (sprintf "(bool)(%s)")
        | AppExp(FreeVarExp(ValName "boolToZahl", _), e) -> bindCXXCode (go env e) [] (sprintf "(int64_t)(%s)")
        | VarExp i -> cxxExpr env.[i]
        | FreeVarExp(ValName x, _) -> cxxExpr x
        | LamExp _ as e -> failwithf "failed to transpile: %A" e
        | AppExp _ as e ->
            let rec loop args sentences =
                function
                | AppExp(e1, e2) ->
                    let e2 = go env e2
                    loop (e2.expr :: args) (List.append sentences e2.sentences) e1
                | FreeVarExp(ValName name, _) ->
                    let s =
                        match name with
                        | _ when name = "+" || name = "-" || name = "*" || name = "/" || name = "%" -> join (" " + name + " ") args
                        | _ when name = "**" -> sprintf "pow(%s)" (join ", " args)
                        | _ -> sprintf "%s[%s]" name (join "][" args)
                    { sentences = sentences
                      expr = s }
                | _ -> failwith "failed to transpile"
            loop [] [] e
        | IfThenElseExp(e1, e2, e3) ->
            let e1 = go env e1
            let e2 = go env e2
            let e3 = go env e3
            if e2.sentences = [] && e3.sentences = [] then bindCXXCode e1 [] (fun e1 -> sprintf "(%s ? %s : %s)" e1 e2.expr e3.expr)
            else
                let sentences = ref e1.sentences
                let line s = sentences := s :: !sentences
                let t = gensym_general()
                line (sprintf "auto %s = ([&]() {" t)
                line (sprintf "if (%s) {" e1.expr)
                sentences := List.append e2.sentences !sentences
                line (sprintf "return %s;" e2.expr)
                line (sprintf "} else {")
                sentences := List.append e3.sentences !sentences
                line (sprintf "return %s;" e3.expr)
                line (sprintf "}")
                { sentences = !sentences
                  expr = t }
        | IntExp n -> cxxExpr (sprintf "%A" n)
        | BoolExp p ->
            cxxExpr (if p then "true"
                     else "false")
    go []

let indent (nest : int) : string = String.replicate nest "    "

let appendLine (sb : StringBuilder) (nest : int) (s : string) : unit =
    let _ = sb.Append(indent nest)
    let _ = sb.Append s

    let _ =
        sb.Append "\n"
    ()

let appendCXXCode (sb : StringBuilder) (nest : int) (code : CXXCode) (cont : string -> string) : unit =
    let rec go (nest : int) : list<string> -> unit =
        function
        | [] -> appendLine sb nest (cont code.expr)
        | sentence :: sentences ->
            let nest =
                (if sentence.StartsWith "}" then nest - 1
                 else nest)
            appendLine sb nest sentence
            let nest =
                (if sentence.EndsWith "{" then nest + 1
                 else nest)
            go nest sentences
    go nest (List.rev code.sentences)

let transpile (given : list<ValName * RType>) (definition : list<ValName * Expr * Schema<RType>>) (e : Expr) (scm : Schema<RType>) : string =
    let sb = new StringBuilder()
    let gensym_accumulator = newGensym id "a"
    let gensym_counter = newGensym id "i"
    let gensym_general = newGensym id "t"
    let _ = sb.Append(getCXXTypeFromSchema scm)
    let _ = sb.Append " solve("
    for (i, (ValName x, t)) in List.indexed (List.rev given) do
        let _ =
            sb.Append(if i = 0 then ""
                      else ", ")

        let _ = sb.Append(getCXXArgumentType t)
        let _ = sb.Append " "
        let _ = sb.Append x
        ()
    let _ =
        sb.Append ") {\n"
    for (ValName x, e, scm) in List.rev definition do
        appendCXXCode sb 1 (transpileExpr gensym_counter gensym_accumulator gensym_general e) (fun e -> sprintf "%s %s = %s;" (getCXXTypeFromSchema scm) x e)
    appendCXXCode sb 1 (transpileExpr gensym_counter gensym_accumulator gensym_general e) (sprintf "return %s;")
    let _ =
        sb.Append "}\n"
    sb.ToString()
