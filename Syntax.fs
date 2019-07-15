module Syntax

open System.Numerics
open Semantics

// parsed types
type PType =
    | FunPTy of PType * PType
    | ZahlPTy
    | NatPTy
    | BoolPTy
    | FinitePTy of IntExpr

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
    | LetGiven of ValName * list<Param> * PType

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

let rec convertFromParsedType : PType -> BType =
    function
    | FunPTy(s, t) -> FunBTy(convertFromParsedType s, convertFromParsedType t)
    | ZahlPTy -> ZahlBTy
    | NatPTy -> NatBTy
    | BoolPTy -> BoolBTy
    | FinitePTy e -> FiniteBTy e

let convertFromParsedExpr (gensym_t : unit -> TyName) (gensym_v : unit -> ValName) (typeenv : Map<ValName, Schema<BType>>) : PExpr -> UExpr =
    let rec go (stk : list<option<ValName>>) =
        function
        | VarPExp x ->
            match List.tryFindIndex (fun y -> y = Some x) stk with
            | Some i -> VarUExp i
            | None ->
                match Map.tryFind x typeenv with
                | None -> failwithf "undefined symbol: %A" x
                | Some scm -> FreeVarUExp(x, realizeSchema gensym_t scm)
        | LamPExp(x, t, e) -> LamUExp(Option.map convertFromParsedType t, go (x :: stk) e)
        | AppPExp(e1, e2) -> AppUExp(go stk e1, go stk e2)
        | IfThenElsePExp(e1, e2, e3) -> IfThenElseUExp(go stk e1, go stk e2, go stk e3)
        | IntPExp n -> IntUExp n
        | BoolPExp p -> BoolUExp p
    go []
