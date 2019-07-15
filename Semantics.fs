module Semantics

open System.Numerics

type ValName = ValName of string

type TyName = TyName of string

// base types
type BType =
    | VarBTy of TyName
    | FunBTy of BType * BType
    | IntBTy

// refined types
type RType<'e> =
    | VarRTy of TyName
    | FunRTy of ValName * RType<'e> * RType<'e>
    | RefineRTy of ValName * BType * 'e

// type schemas
type Schema<'t> =
    | Monotype of 't
    | Polytype of TyName * 't

// untyped exprs
type UExpr =
    | VarUExp of int
    | FreeVarUExp of ValName * RType<UExpr>
    | LamUExp of option<RType<UExpr>> * UExpr
    | AppUExp of UExpr * UExpr
    | LetInPExp of ValName * option<Schema<RType<UExpr>>> * UExpr * UExpr
    | IfThenElsePExp of UExpr * UExpr * UExpr
    | IntUExp of BigInteger

// typed exprs
type Expr =
    | VarExp of int
    | FreeVarExp of ValName * RType<Expr>
    | LamExp of RType<Expr> * Expr
    | AppExp of Expr * Expr
    | LetInExp of ValName * Schema<RType<Expr>> * Expr * Expr
    | IfThenElseExp of Expr * Expr * Expr
    | IntExp of BigInteger
