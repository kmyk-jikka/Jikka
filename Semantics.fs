module Semantics

open System.Numerics

type ValName = ValName of string

type TyName = TyName of string

type IntExpr =
    | ConstIExp of BigInteger
    | VarIExp of ValName
    | AddIExp of ValName * BigInteger
    | MulIExp of BigInteger * ValName
    | MulAddIExp of BigInteger * ValName * BigInteger

// base types
type BType =
    | VarBTy of TyName
    | FunBTy of BType * BType
    | ZahlBTy
    | NatBTy
    | BoolBTy
    | FiniteBTy of IntExpr

// type schemas
type Schema<'t> =
    | Monotype of 't
    | Polytype of TyName * Schema<'t>

// untyped exprs
type UExpr =
    | VarUExp of int
    | FreeVarUExp of ValName * BType
    | LamUExp of option<BType> * UExpr
    | AppUExp of UExpr * UExpr
    | IfThenElseUExp of UExpr * UExpr * UExpr
    | IntUExp of BigInteger
    | BoolUExp of bool

// typed exprs
type Expr =
    | VarExp of int
    | FreeVarExp of ValName * BType
    | LamExp of BType * Expr
    | AppExp of Expr * Expr
    | IfThenElseExp of Expr * Expr * Expr
    | IntExp of BigInteger
    | BoolExp of bool

let newGensym (callback : string -> 'a) (prefix : string) : unit -> 'a =
    let i = ref 0
    fun () ->
        i := !i + 1
        callback (sprintf "%s%d" prefix (!i))

let rec foldSchema (monotype : 't -> 'a) (polytype : TyName -> 'a -> 'a) : Schema<'t> -> 'a =
    let rec go =
        function
        | Monotype t -> monotype t
        | Polytype(x, scm) -> polytype x (go scm)
    go

let substTyVarOfBType (b : TyName) (a : BType) : BType -> BType =
    let rec go =
        function
        | VarBTy x ->
            if x = b then a
            else VarBTy x
        | FunBTy(t1, t2) -> FunBTy(go t1, go t2)
        | ZahlBTy -> ZahlBTy
        | NatBTy -> NatBTy
        | BoolBTy -> BoolBTy
        | FiniteBTy n -> FiniteBTy n
    go

let substTyVarOfConstraints (b : TyName) (a : BType) : list<BType * BType> -> list<BType * BType> = List.map (fun (t1, t2) -> (substTyVarOfBType b a t1, substTyVarOfBType b a t2))
let substTyVarsOfBType (subst : list<TyName * BType>) : BType -> BType = List.foldBack (fun (b, a) t -> substTyVarOfBType b a t) subst

let substTyVarOfExpr (b : TyName) (a : BType) : Expr -> Expr =
    let subst = substTyVarOfBType b a

    let rec go =
        function
        | VarExp x -> VarExp x
        | FreeVarExp(x, t) -> FreeVarExp(x, subst t)
        | LamExp(t, e) -> LamExp(subst t, go e)
        | AppExp(e1, e2) -> AppExp(go e1, go e2)
        | IfThenElseExp(e1, e2, e3) -> IfThenElseExp(go e1, go e2, go e3)
        | IntExp n -> IntExp n
        | BoolExp p -> BoolExp p
    go

let substTyVarsOfExpr (subst : list<TyName * BType>) : Expr -> Expr = List.foldBack (fun (b, a) e -> substTyVarOfExpr b a e) subst
let realizeSchema (gensym : unit -> TyName) : Schema<BType> -> BType = foldSchema id (fun x t -> substTyVarOfBType x (VarBTy(gensym())) t)

let listTypeConstraints (gensym : unit -> TyName) : UExpr -> (Expr * BType * list<BType * BType>) =
    let rec go (stk : list<BType>) (acc : list<BType * BType>) =
        function
        | VarUExp i -> (VarExp i, stk.[i], acc)
        | FreeVarUExp(x, t) -> (FreeVarExp(x, t), t, acc)
        | LamUExp(t, e) ->
            let t =
                match t with
                | None -> VarBTy(gensym())
                | Some t -> t

            let (e, u, acc) = go (t :: stk) acc e
            (LamExp(t, e), FunBTy(t, u), acc)
        | AppUExp(e1, e2) ->
            let (e1, t1, acc) = go stk acc e1
            let (e2, t2, acc) = go stk acc e2
            let s = VarBTy(gensym())
            (AppExp(e1, e2), s, (t1, FunBTy(t2, s)) :: acc)
        | IfThenElseUExp(e1, e2, e3) ->
            let (e1, t1, acc) = go stk acc e1
            let (e2, t2, acc) = go stk acc e2
            let (e3, t3, acc) = go stk acc e3
            (IfThenElseExp(e1, e2, e3), t2, (t1, BoolBTy) :: (t2, t3) :: acc)
        | IntUExp n ->
            let t =
                if n >= 0I then FiniteBTy(ConstIExp(n + 1I))
                else ZahlBTy
            (IntExp n, t, acc)
        | BoolUExp p -> (BoolExp p, BoolBTy, acc)
    go [] []

let listFreeTyVarsOfBType (env : list<TyName>) : BType -> list<TyName> =
    let rec go acc t =
        match t with
        | VarBTy x ->
            if List.contains x env then acc
            else x :: acc
        | FunBTy(s, t) -> go (go acc s) t
        | ZahlBTy -> acc
        | NatBTy -> acc
        | BoolBTy -> acc
        | FiniteBTy n -> acc
    go []

let listFreeTyVarsOfSchema (env : list<TyName>) (scm : Schema<BType>) : list<TyName> =
    let (evn, t) = foldSchema (fun t -> ([], t)) (fun x (env, t) -> (x :: env, t)) scm
    listFreeTyVarsOfBType env t

let rec isSubtype (t1 : BType) (t2 : BType) : bool =
    match (t1, t2) with
    | _ when t1 = t2 -> true
    | (NatBTy, ZahlBTy) -> true
    | (FiniteBTy _, ZahlBTy) -> true
    | (FiniteBTy _, NatBTy) -> true
    | (FunBTy(t11, t12), FunBTy(t21, t22)) when isSubtype t21 t11 && isSubtype t12 t22 -> true
    | _ -> false

let rec unifyConstraints : list<BType * BType> -> list<TyName * BType> =
    function
    | [] -> []
    | (t1, t2) :: constraints ->
        match (t1, t2) with
        | _ when t1 = t2 -> unifyConstraints constraints
        | _ when isSubtype t1 t2 -> unifyConstraints constraints
        | _ when isSubtype t2 t1 -> unifyConstraints constraints
        | (VarBTy x, t) when not (List.contains x (listFreeTyVarsOfBType [] t)) ->
            let subst = unifyConstraints (substTyVarOfConstraints x t constraints)
            (x, substTyVarsOfBType subst t) :: subst
        | (t, VarBTy x) when not (List.contains x (listFreeTyVarsOfBType [] t)) ->
            let subst = unifyConstraints (substTyVarOfConstraints x t constraints)
            (x, substTyVarsOfBType subst t) :: subst
        | (FunBTy(t11, t12), FunBTy(t21, t22)) -> unifyConstraints ((t11, t21) :: (t12, t22) :: constraints)
        | (_, _) -> failwithf "failed to unify constraints: %A = %A" t1 t2

// Hindley/Milner type inference
let inferTypes (gensym : unit -> TyName) (e : UExpr) (annot : option<BType>) : Expr * Schema<BType> =
    let (e, t, constraints) = listTypeConstraints gensym e

    let constraints =
        match annot with
        | None -> constraints
        | Some annot -> (t, annot) :: constraints
    printfn "e = %A" e
    printfn "t = %A" t
    printfn "constraints = %A" constraints
    let subst = unifyConstraints constraints
    printfn "subst = %A" subst
    let e = substTyVarsOfExpr subst e
    let t = substTyVarsOfBType subst t
    let scm = Monotype t
    match listFreeTyVarsOfSchema [] scm with
    | [] -> ()
    | _ -> failwithf "the type schema is not closed: %A" scm
    (e, scm)
