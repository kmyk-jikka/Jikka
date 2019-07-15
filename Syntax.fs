module Syntax

open System.Numerics
open Semantics

// parsed exprs
type PExpr =
    | VarPExp of ValName
    | LamPExp of option<ValName> * option<PType> * PExpr
    | AppPExp of PExpr * PExpr
    | LetInPExp of option<ValName> * option<PType> * PExpr * PExpr
    | IfThenElsePExp of PExpr * PExpr * PExpr
    | IntPExp of BigInteger

// parsed types
and PType =
    | VarPTy of TyName
    | FunPTy of option<ValName> * PType * PType
    | RefinePTy of option<ValName> * option<PType> * PExpr

type Param = Param of option<ValName> * option<PType>

type Pattern =
    | VarPat of option<ValName> * option<PType>
    | IntPat of BigInteger * option<PType>
    | PlusKPat of option<ValName> * BigInteger * option<PType>

type Declaration =
    | Let of ValName * list<Param> * option<PType> * PExpr
    | LetRec of ValName * option<PType> * list<list<Pattern> * PExpr>
    | LetGiven of ValName * list<Param> * option<PType>

type Program =
    { toplevel : list<Declaration>
      expr : PExpr }
