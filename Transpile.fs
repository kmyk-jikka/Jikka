module Jikka.Transpile

open Jikka.Semantics
open System.Text

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

let getCXXDeclType : RType -> string =
    function
    | FunRTy _ -> "auto &"
    | t -> getCXXType t

let getMonotype : Schema<'t> -> 't =
    function
    | Polytype _ as scm -> failwithf "not a monotype: %A" scm
    | Monotype t -> t

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

let transpileIntExpr (toplevel : list<Defined>) : IntExpr -> string =
    let rec go prec =
        let paren n s =
            if prec < n then "(" + s + ")"
            else s
        function
        | LiteralIExp n -> paren 1 (sprintf "%A" n)
        | VarIExp(ValName x) -> paren 1 x
        | NegateIExp e -> paren 3 (sprintf "- %s" (go 3 e))
        | AddIExp(e1, e2) -> paren 5 (sprintf "%s + %s" (go 5 e1) (go 5 e2))
        | SubIExp(e1, e2) -> paren 5 (sprintf "%s - %s" (go 5 e1) (go 5 e2))
        | MulIExp(e1, e2) -> paren 4 (sprintf "%s * %s" (go 4 e1) (go 4 e2))
        | DivIExp(e1, e2) -> paren 4 (sprintf "%s / %s" (go 4 e1) (go 4 e2))
        | ModIExp(e1, e2) -> paren 4 (sprintf "%s %% %s" (go 4 e1) (go 4 e2))
        | PowIExp(e1, e2) -> paren 2 (sprintf "pow(%s, %s)" (go 9 e1) (go 9 e2))
    go 9

let transpileExpr (toplevel : list<Defined>) (gensym_counter : unit -> string) (gensym_accumulator : unit -> string) (gensym_general : unit -> string) : Expr -> CXXCode =
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
        | LamExp _ as e ->
            let sentences = ref []
            let line s = sentences := s :: !sentences
            let ary = gensym_general()
            match e with
            | LamExp(OrdinalRTy n1, LamExp(OrdinalRTy n2, LamExp(OrdinalRTy n3, body))) ->
                let t =
                    getCXXType (getType [ OrdinalRTy n3
                                          OrdinalRTy n2
                                          OrdinalRTy n1 ] body)

                let n1 = transpileIntExpr toplevel n1
                let n2 = transpileIntExpr toplevel n2
                let n3 = transpileIntExpr toplevel n3
                let i1 = gensym_counter()
                let i2 = gensym_counter()
                let i3 = gensym_counter()
                line (sprintf "vector<vector<vector<%s> > > %s(%s, vector<vector<%s> >(%s, vector<%s>(%s)));" t ary n1 t n2 t n3)
                line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i1 i1 n1 i1)
                line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i2 i2 n2 i2)
                line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i3 i3 n3 i3)
                let body = go (i3 :: i2 :: i1 :: env) body
                sentences := List.append body.sentences !sentences
                line (sprintf "%s[%s][%s][%s] = %s;" ary i1 i2 i3 body.expr)
                line "}"
                line "}"
                line "}"
            | LamExp(OrdinalRTy n1, LamExp(OrdinalRTy n2, body)) ->
                let t =
                    getCXXType (getType [ OrdinalRTy n2
                                          OrdinalRTy n1 ] body)

                let n1 = transpileIntExpr toplevel n1
                let n2 = transpileIntExpr toplevel n2
                let i1 = gensym_counter()
                let i2 = gensym_counter()
                line (sprintf "vector<vector<%s> > %s(%s, vector<%s>(%s));" t ary n1 t n2)
                line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i1 i1 n1 i1)
                line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i2 i2 n2 i2)
                let body = go (i2 :: i1 :: env) body
                sentences := List.append body.sentences !sentences
                line (sprintf "%s[%s][%s] = %s;" ary i1 i2 body.expr)
                line "}"
                line "}"
            | LamExp(OrdinalRTy n1, body) ->
                let t = getCXXType (getType [ OrdinalRTy n1 ] body)
                let n1 = transpileIntExpr toplevel n1
                let i1 = gensym_counter()
                line (sprintf "vector<%s> %s(%s);" t ary n1)
                line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i1 i1 n1 i1)
                let body = go (i1 :: env) body
                sentences := List.append body.sentences !sentences
                line (sprintf "%s[%s] = %s;" ary i1 body.expr)
                line "}"
            | _ -> failwithf "failed to transpile: %A" e
            { sentences = !sentences
              expr = ary }
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

let transpile (toplevel : list<Defined>) (e : Expr) (scm : Schema<RType>) : string =
    let sb = new StringBuilder()
    let gensym_accumulator = newGensym id "a"
    let gensym_counter = newGensym id "i"
    let gensym_general = newGensym id "t"
    let _ = sb.Append(getCXXType (getMonotype scm))
    let _ = sb.Append " solve("
    for (i, (ValName x, t)) in List.indexed (List.rev (filterGiven toplevel)) do
        let _ =
            sb.Append(if i = 0 then ""
                      else ", ")

        let _ = sb.Append(getCXXArgumentType t)
        let _ = sb.Append " "
        let _ = sb.Append x
        ()
    let _ =
        sb.Append ") {\n"
    for (ValName x, e, scm) in List.rev (filterDefined toplevel) do
        let e = transpileExpr toplevel gensym_counter gensym_accumulator gensym_general e
        appendCXXCode sb 1 e (fun e -> sprintf "%s %s = %s;" (getCXXDeclType (getMonotype scm)) x e)
    let e = transpileExpr toplevel gensym_counter gensym_accumulator gensym_general e
    appendCXXCode sb 1 e (sprintf "return %s;")
    let _ =
        sb.Append "}\n"
    sb.ToString()
