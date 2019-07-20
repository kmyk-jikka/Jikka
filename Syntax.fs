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

type Pattern =
    | VarPat of option<ValName> * option<PType>
    | IntPat of BigInteger * option<PType>
    | PlusKPat of option<ValName> * BigInteger * option<PType>

type Declaration =
    | Let of ValName * list<Param> * option<PType> * PExpr
    | LetRec of ValName * option<PType> * list<list<Pattern> * PExpr>
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

let convertFromParsedExpr (gensym_t : unit -> TyName) (gensym_v : unit -> ValName) (typeenv : list<ValName * Schema<RType>>) : PExpr -> Expr =
    let rec go (stk : list<option<ValName>>) =
        function
        | VarPExp x ->
            match List.tryFindIndex (fun y -> y = Some x) stk with
            | Some i -> VarExp i
            | None ->
                match List.tryFind (fun (y, _) -> y = x) typeenv with
                | None -> failwithf "undefined symbol: %A" x
                | Some(_, scm) -> FreeVarExp(x, realizeSchema gensym_t scm)
        | LamPExp(x, t, e) ->
            match t with
            | None -> LamExp(VarRTy(gensym_t()), go (x :: stk) e)
            | Some t -> LamExp(convertFromParsedType t, go (x :: stk) e)
        | AppPExp(e1, e2) -> AppExp(go stk e1, go stk e2)
        | IfThenElsePExp(e1, e2, e3) -> IfThenElseExp(go stk e1, go stk e2, go stk e3)
        | IntPExp n -> IntExp n
        | BoolPExp p -> BoolExp p
    go []

let lambdaFromParam (x : Param) (e : PExpr) : PExpr =
    let (Param(x, t)) = x
    LamPExp(x, t, e)
