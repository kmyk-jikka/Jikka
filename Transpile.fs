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

type CXXCode<'a> =
    { sentences : list<string>
      expr : 'a }

type CXXCodeBuilder() =

    member __.Return(x) =
        { sentences = []
          expr = x }

    member __.ReturnFrom(x) = x

    member __.Bind(x, f) =
        let y = f x.expr
        { sentences = List.append y.sentences x.sentences
          expr = y.expr }

    member __.Combine(x, y) = __.Bind(x, fun _ -> y)
    member __.Delay(f) = f()
    member __.For(xs, f) = Seq.fold (fun acc x -> __.Combine(acc, f x)) (__.Zero()) xs
    member __.Zero() = __.Return(())

let cxxcode = new CXXCodeBuilder()

let line (s : string) : CXXCode<unit> =
    { sentences = [ s ]
      expr = () }

let bindCXXCode (code : CXXCode<string>) (sentences : list<string>) (expr : string -> string) =
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

let transpileExpr (toplevel : list<Defined>) (gensym_counter : unit -> string) (gensym_accumulator : unit -> string) (gensym_general : unit -> string) : Expr -> CXXCode<string> =
    let rec go (env : list<string>) : Expr -> CXXCode<string> =
        let accumulate plus zero l r e =
            cxxcode {
                let! e = match e with
                         | LamExp(_, e) -> cxxcode { return (fun i -> go (i :: env) e) }
                         | _ ->
                             cxxcode { let! e = go env e
                                       return (fun i -> cxxcode { return sprintf "%s[%s]" e i }) }
                let acc = gensym_accumulator()
                do! line (sprintf "int64_t %s = %s;" acc zero)
                let i = gensym_counter()
                let! l = go env l
                let! r = go env r
                do! line (sprintf "for (int64_t %s = %s; %s < %s; ++ %s) {" i l i r i)
                let! e = e i
                do! line (plus acc e + ";")
                do! line "}"
                return acc
            }
        function
        | AppExp(AppExp(FreeVarExp(ValName "count", _), n), e) -> accumulate (sprintf "%s += (bool)(%s)") "0" (IntExp 0I) n e
        | AppExp(AppExp(FreeVarExp(ValName "sum", _), n), e) -> accumulate (sprintf "%s += %s") "0" (IntExp 0I) n e
        | AppExp(AppExp(FreeVarExp(ValName "max", _), n), e) -> accumulate (fun a b -> sprintf "%s = max<int64_t>(%s, %s)" a a b) "INT64_MIN" (IntExp 0I) n e
        | AppExp(AppExp(FreeVarExp(ValName "min", _), n), e) -> accumulate (fun a b -> sprintf "%s = min<int64_t>(%s, %s)" a a b) "INT64_MAX" (IntExp 0I) n e
        | AppExp(AppExp(AppExp(FreeVarExp(ValName "count3", _), l), r), e) -> accumulate (sprintf "%s += (bool)(%s)") "0" l r e
        | AppExp(AppExp(AppExp(FreeVarExp(ValName "sum3", _), l), r), e) -> accumulate (sprintf "%s += %s") "0" l r e
        | AppExp(AppExp(AppExp(FreeVarExp(ValName "max3", _), l), r), e) -> accumulate (fun a b -> sprintf "%s = max<int64_t>(%s, %s)" a a b) "INT64_MIN" l r e
        | AppExp(AppExp(AppExp(FreeVarExp(ValName "min3", _), l), r), e) -> accumulate (fun a b -> sprintf "%s = min<int64_t>(%s, %s)" a a b) "INT64_MAX" l r e
        | AppExp(FreeVarExp(ValName "negate", _), e) ->
            cxxcode { let! e = go env e
                      return sprintf "(- %s)" e }
        | AppExp(FreeVarExp(ValName "!", _), e) ->
            cxxcode { let! e = go env e
                      return sprintf "(! %s)" e }
        | AppExp(FreeVarExp(ValName "zahlToBool", _), e) ->
            cxxcode { let! e = go env e
                      return sprintf "(bool)(%s)" e }
        | AppExp(FreeVarExp(ValName "boolToZahl", _), e) ->
            cxxcode { let! e = go env e
                      return sprintf "(int64_t)(%s)" e }
        | VarExp i -> cxxcode { return env.[i] }
        | FreeVarExp(ValName x, _) -> cxxcode { return x }
        | LamExp _ as e ->
            cxxcode {
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
                    do! line (sprintf "vector<vector<vector<%s> > > %s(%s, vector<vector<%s> >(%s, vector<%s>(%s)));" t ary n1 t n2 t n3)
                    do! line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i1 i1 n1 i1)
                    do! line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i2 i2 n2 i2)
                    do! line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i3 i3 n3 i3)
                    let! body = go (i3 :: i2 :: i1 :: env) body
                    do! line (sprintf "%s[%s][%s][%s] = %s;" ary i1 i2 i3 body)
                    do! line "}"
                    do! line "}"
                    do! line "}"
                | LamExp(OrdinalRTy n1, LamExp(OrdinalRTy n2, body)) ->
                    let t =
                        getCXXType (getType [ OrdinalRTy n2
                                              OrdinalRTy n1 ] body)

                    let n1 = transpileIntExpr toplevel n1
                    let n2 = transpileIntExpr toplevel n2
                    let i1 = gensym_counter()
                    let i2 = gensym_counter()
                    do! line (sprintf "vector<vector<%s> > %s(%s, vector<%s>(%s));" t ary n1 t n2)
                    do! line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i1 i1 n1 i1)
                    do! line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i2 i2 n2 i2)
                    let! body = go (i2 :: i1 :: env) body
                    do! line (sprintf "%s[%s][%s] = %s;" ary i1 i2 body)
                    do! line "}"
                    do! line "}"
                | LamExp(OrdinalRTy n1, body) ->
                    let t = getCXXType (getType [ OrdinalRTy n1 ] body)
                    let n1 = transpileIntExpr toplevel n1
                    let i1 = gensym_counter()
                    do! line (sprintf "vector<%s> %s(%s);" t ary n1)
                    do! line (sprintf "for (int %s = 0; %s < %s; ++ %s) {" i1 i1 n1 i1)
                    let! body = go (i1 :: env) body
                    do! line (sprintf "%s[%s] = %s;" ary i1 body)
                    do! line "}"
                | _ -> failwithf "failed to transpile: %A" e
                return ary
            }
        | AppExp _ as e ->
            let rec loop args =
                function
                | AppExp(e1, e2) ->
                    cxxcode { let! e2 = go env e2
                              return! loop (e2 :: args) e1 }
                | VarExp i -> cxxcode { return sprintf "%s[%s]" env.[i] (join "][" args) }
                | FreeVarExp(ValName name, _) ->
                    let s =
                        match name with
                        | _ when name = "<" || name = "<=" || name = ">" || name = ">=" -> "(" + join (" " + name + " ") args + ")"
                        | _ when name = "=" -> "(" + join (" == ") args + ")"
                        | _ when name = "<>" -> "(" + join (" != ") args + ")"
                        | _ when name = "+" || name = "-" || name = "*" || name = "/" || name = "%" -> "(" + join (" " + name + " ") args + ")"
                        | _ when name = "**" -> sprintf "pow(%s)" (join ", " args)
                        | _ when name = "&&" || name = "||" -> "(" + join (" " + name + " ") args + ")"
                        | _ when name = "max2" -> sprintf "max<int64_t>(%s)" (join ", " args)
                        | _ when name = "min2" -> sprintf "min<int64_t>(%s)" (join ", " args)
                        | _ -> sprintf "%s[%s]" name (join "][" args)
                    cxxcode { return s }
                | _ -> failwithf "failed to transpile in %A" e
            loop [] e
        | FixpoExp(s, patterns, ts) -> failwith "not implemented yet"
        | InductionExp(t1, t2, bases, step) as e ->
            match t1 with
            | OrdinalRTy n ->
                cxxcode {
                    let ary = gensym_general()
                    let n = transpileIntExpr toplevel n
                    let i = gensym_counter()
                    do! line (sprintf "vector<%s> %s(%s);" (getCXXType t2) ary n)
                    for (k, base_) in List.indexed bases do
                        let! base_ = go env base_
                        do! line (sprintf "%s[%d] = %s;" ary k base_)
                        return ()
                    do! line (sprintf "for (int %s = 0; %s < %s - %d; ++ %s) {" i i n (List.length bases) i)
                    let! step = go (i :: ary :: env) step
                    do! line (sprintf "%s[%s + %d] = %s;" ary i (List.length bases) step)
                    do! line "}"
                    return ary
                }
            | _ -> failwithf "failed to transpile: %A" e
        | IfThenElseExp(e1, e2, e3) ->
            cxxcode {
                let! e1 = go env e1
                let e2 = go env e2
                let e3 = go env e3
                if e2.sentences = [] && e3.sentences = [] then return sprintf "(%s ? %s : %s)" e1 e2.expr e3.expr
                else
                    let t = gensym_general()
                    do! line (sprintf "auto %s = ([&]() {" t)
                    do! line (sprintf "if (%s) {" e1)
                    let! e2 = e2
                    do! line (sprintf "return %s;" e2)
                    do! line (sprintf "} else {")
                    let! e3 = e3
                    do! line (sprintf "return %s;" e3)
                    do! line (sprintf "}")
                    return t
            }
        | IntExp n -> cxxcode { return sprintf "%A" n }
        | BoolExp p ->
            cxxcode {
                return if p then "true"
                       else "false"
            }
    go []

let indent (nest : int) : string = String.replicate nest "    "

let appendLine (sb : StringBuilder) (nest : int) (s : string) : unit =
    let _ = sb.Append(indent nest)
    let _ = sb.Append s

    let _ =
        sb.Append "\n"
    ()

let appendCXXCode (sb : StringBuilder) (nest : int) (code : CXXCode<string>) (cont : string -> string) : unit =
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
