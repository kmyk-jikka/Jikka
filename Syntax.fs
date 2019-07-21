module Jikka.Syntax

open Jikka.Semantics
open System.Numerics

// parsed types
type PType =
    | FunPTy of PType * PType
    | ZahlPTy
    | NatPTy
    | OrdinalPTy of IntExpr
    | RangePTy of IntExpr * IntExpr
    | BoolPTy

// parsed exprs
and PExpr =
    | VarPExp of ValName
    | LamPExp of option<ValName> * option<PType> * PExpr
    | AppPExp of PExpr * PExpr
    | IfThenElsePExp of PExpr * PExpr * PExpr
    | IntPExp of BigInteger
    | BoolPExp of bool

type Param = Param of option<ValName> * option<PType>

type PPattern =
    | VarPPat of option<ValName> * option<PType>
    | BoolPPat of bool * option<PType>
    | IntPPat of BigInteger * option<PType>
    | PlusKPPat of option<ValName> * BigInteger * option<PType>

type Declaration =
    | Let of ValName * list<Param> * option<PType> * PExpr
    | LetRec of ValName * option<PType> * list<list<PPattern> * PExpr>
    | LetGiven of ValName * PType

type Program =
    { toplevel : list<Declaration>
      expr : PExpr }

let listFreeVarsOfPExpr : PExpr -> list<ValName> =
    let rec go acc =
        function
        | VarPExp x -> x :: acc
        | LamPExp(x, _, e) -> List.filter (fun y -> Some y <> x) (go acc e)
        | AppPExp(e1, e2) -> go (go acc e1) e2
        | IfThenElsePExp(e1, e2, e3) -> go (go (go acc e1) e2) e3
        | IntPExp _ -> acc
        | BoolPExp _ -> acc
    go []

let rec convertFromParsedType : PType -> RType =
    function
    | FunPTy(s, t) -> FunRTy(convertFromParsedType s, convertFromParsedType t)
    | ZahlPTy -> ZahlRTy
    | NatPTy -> NatRTy
    | OrdinalPTy e -> OrdinalRTy e
    | RangePTy(l, r) -> RangeRTy(l, r)
    | BoolPTy -> BoolRTy

let convertFromParsedExpr (gensym_t : unit -> TyName) (gensym_v : unit -> ValName) (typeenv : list<ValName * Schema<RType>>) : list<option<ValName>> -> PExpr -> Expr =
    let getRType =
        function
        | None -> VarRTy(gensym_t())
        | Some t -> convertFromParsedType t

    let rec go (stk : list<option<ValName>>) =
        function
        | VarPExp x ->
            match List.tryFindIndex (fun y -> y = Some x) stk with
            | Some i -> VarExp i
            | None ->
                match List.tryFind (fun (y, _) -> y = x) typeenv with
                | None -> failwithf "undefined symbol: %A" x
                | Some(_, scm) -> FreeVarExp(x, realizeSchema gensym_t scm)
        | LamPExp(x, t, e) -> LamExp(getRType t, go (x :: stk) e)
        | AppPExp(e1, e2) -> AppExp(go stk e1, go stk e2)
        | IfThenElsePExp(e1, e2, e3) -> IfThenElseExp(go stk e1, go stk e2, go stk e3)
        | IntPExp n -> IntExp n
        | BoolPExp p -> BoolExp p

    go

let convertFromParsedPatterns (gensym_t : unit -> TyName) (gensym_v : unit -> ValName) (typeenv : list<ValName * Schema<RType>>) (name : ValName) (annot : option<PType>) (patterns : list<list<PPattern> * PExpr>) : Expr * RType =
    let getRType =
        function
        | None -> VarRTy(gensym_t())
        | Some t -> convertFromParsedType t

    let annot = getRType annot

    let head =
        match patterns with
        | (head, _) :: _ -> head
        | _ -> failwithf "no definitions: %A" name

    let put pattern t (patterns, e, ts) = (pattern :: patterns, e, t :: ts)

    let rec convertExprs stk patterns e =
        match patterns with
        | [] -> ([], convertFromParsedExpr gensym_t gensym_v typeenv stk e, [])
        | VarPPat(x, t) :: patterns -> put VarPat t (convertExprs (x :: stk) patterns e)
        | BoolPPat(p, t) :: patterns -> put (BoolPat p) t (convertExprs (None :: stk) patterns e)
        | IntPPat(n, t) :: patterns -> put (IntPat n) t (convertExprs (None :: stk) patterns e)
        | PlusKPPat(x, k, t) :: patterns -> put (PlusKPat k) t (convertExprs (x :: stk) patterns e)

    let unifyPTypes (ts : list<option<PType>>) : RType =
        match List.distinct (List.choose id ts) with
        | [] -> VarRTy(gensym_t())
        | t :: [] -> convertFromParsedType t
        | _ -> failwithf "inconsistent type restrictions found: %A" name

    let rec transposePTypes (xss : list<list<option<PType>>>) : list<RType> =
        let hasNil = List.tryFind (fun xs -> xs = []) xss <> None
        let hasCons = List.tryFind (fun xs -> xs <> []) xss <> None
        if hasNil then
            if hasCons then failwithf "ununiform numbers of patterns: %A" name
            else []
        else unifyPTypes (List.map List.head xss) :: transposePTypes (List.map List.tail xss)

    let patterns = List.map (fun (patterns, e) -> convertExprs [ Some name ] patterns e) patterns
    let ts = transposePTypes (List.map (fun (_, _, ts) -> ts) patterns)
    let patterns = List.map (fun (patterns, e, _) -> (patterns, e)) patterns
    (FixpoExp(annot, patterns, ts), annot)

let lambdaFromParam (x : Param) (e : PExpr) : PExpr =
    let (Param(x, t)) = x
    LamPExp(x, t, e)
