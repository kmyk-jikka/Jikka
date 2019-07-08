module Syntax

open Semantics

type Assumption
    = Define of Ident * list<option<Ident>> * UExpr
    | Declare of Ident * BSchema
    | Input of Ident * BType
    | Assume of UExpr

type Program =
    { environment : list<Assumption>
    ; output : UExpr
    }
