module Syntax

open System.Numerics

type Ident = Ident of string

type Expr
    = IdentExp of Ident
    | IntExp of BigInteger
    | LamExp of Ident * Expr
    | AppExp of Expr * Expr

type Type
    = ExprTy of Expr
    | FunTy of Type * Type
    | AppTy of Type * Type

type Assumption
    = Define of Ident * list<option<Ident>> * Expr
    | Declare of Ident * Type
    | Given of Ident * Type
    | Assume of Expr

type Program =
    { environment : list<Assumption>
    ; compute : Expr
    }

let identExp s = IdentExp (Ident s)
let appExp2 f x y = AppExp (AppExp (f, x), y)
