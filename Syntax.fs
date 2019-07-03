module Syntax

type Ident = Ident of string

type Expr
    = IdentExp of Ident
    | IntExp of int
    | FunExp of Ident * Expr
    | AppEp of Expr * Expr

type Type
    = IdentTy of Ident
    | LamTy of Type * Type
    | AppTy of Type * Type

type Assumption
    = Define of Ident * Expr
    | Given of Ident * Type
    | Assume of Expr

type Program =
    { environment : list<Assumption>
    ; compute : Expr
    }
