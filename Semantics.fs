module Semantics

open System.Numerics

type Ident = Ident of string


// base types
type BType
    = VarBTy of Ident
    | FunBTy of BType * BType
    | IntBTy

// base type schema
type BSchema
    = BaseBScm of BType
    | LamBScm of Ident * BSchema

// untyped expr
type UExpr
    = VarUExp of Ident
    | LamUExp of Ident * UExpr
    | AppUExp of UExpr * UExpr
    | AsUExp of UExpr * BType
    | LamAsUExp of Ident * BType * UExpr
    | IntUExp of BigInteger

// de Bruijn indexed untyped expr
type DBExpr
    = VarDBExp of int
    | LamDBExp of DBExpr
    | AppDBExp of DBExpr * DBExpr
    | AsDBExp of DBExpr * BType
    | LamAsDBExp of BType * DBExpr
    | IntDBExp of BigInteger

// base typed expr
type BExpr
    = VarBExp of int
    | LamBExp of BType * BExpr
    | AppBExp of BExpr * BExpr
    | IntBExp of BigInteger

let convertDeBruijnIndex (f : UExpr) : DBExpr =
    let rec go (env : list<Ident>) f =
        match f with
        | VarUExp x ->
            let i = List.findIndex (fun y -> x = y) env  // or System.Collections.Generic.KeyNotFoundException raised; TODO: use List.tryFindIndex
            VarDBExp i
        | LamUExp (x, g) -> LamDBExp (go (x :: env) g)
        | AppUExp (g, h) -> AppDBExp (go env g, go env h)
        | AsUExp (g, t) -> AsDBExp (go env g, t)
        | LamAsUExp (x, t, g) -> LamAsDBExp (t, go (x :: env) g)
        | IntUExp n -> IntDBExp n
    go [] f

let freshIdentGen (prefix : string) : unit -> Ident =
    let i = ref 0
    fun () ->
        i := ! i + 1
        Ident (sprintf "%s%d" prefix (! i))

let listConstraints (f : DBExpr) : BExpr * BType * list<BType * BType> =
    let gensym = freshIdentGen "_t"
    let rec go (env : list<BType>) (f : DBExpr) (acc : list<BType * BType>) =
        match f with
        | VarDBExp i ->
            (VarBExp i, env.[i], acc)
        | LamDBExp g ->
            go env (LamAsDBExp (VarBTy (gensym ()), g)) acc
        | AppDBExp (g, h) ->
            let (g, t, acc) = go env g acc
            let (h, u, acc) = go env h acc
            let s = VarBTy (gensym ())
            (AppBExp (g, h), s, (t, FunBTy (u, s)) :: acc)
        | AsDBExp (g, t) ->
            let (g, u, acc) = go env g acc
            (g, u, (u, t) :: acc)
        | LamAsDBExp (t, g) ->
            let (g, u, acc) = go (t :: env) g acc
            (LamBExp (t, g), FunBTy (t, u), acc)
        | IntDBExp n ->
            (IntBExp n, IntBTy, acc)
    go [] f []

let listFreeTypeVars (env : list<Ident>) : BType -> list<Ident> =
    let rec go acc t =
        match t with
        | VarBTy x ->
            if List.contains x env then
                acc
            else
                x :: acc
        | FunBTy (s, t) -> go (go acc s) t
        | IntBTy -> acc
    go []

// NOTE: substitute non-recursively; substitution of x with [(x, y); (y, z)] returns y, not z
let substTypes (subst : list<Ident * BType>) : BType -> BType =
    let rec go t =
        match t with
        | VarBTy x ->
            match List.tryFind (fun (y, _) -> x = y) subst with
            | Some (_, s) -> s
            | None -> t
        | FunBTy (s, t) -> FunBTy (go s, go t)
        | IntBTy -> IntBTy
    go

let substTypesOfExpr (subst : list<Ident * BType>) : BExpr -> BExpr =
    let rec go f =
        match f with
        | VarBExp i -> VarBExp i
        | LamBExp (t, g) -> LamBExp (substTypes subst t, go g)
        | AppBExp (g, h) -> AppBExp (go g, go h)
        | IntBExp n -> IntBExp n
    go

let substType (x : Ident) (s : BType) : BType -> BType =
    substTypes [(x, s)]

let substConstraints (x : Ident) (s : BType) : list<BType * BType> -> list<BType * BType> =
    List.map (fun (t, u) -> (substType x s t, substType x s u))

let rec unifyConstraints (constraints : list<BType * BType>) : list<Ident * BType> =
    match constraints with
    | [] -> []
    | (s, t) :: constraints ->
        match (s, t) with
        | (VarBTy x, VarBTy y) when x = y ->
            unifyConstraints constraints
        | (VarBTy x, t) when not (List.contains x (listFreeTypeVars [] t)) ->
            let subst = unifyConstraints (substConstraints x t constraints)
            (x, substTypes subst t) :: subst
        | (t, VarBTy x) when not (List.contains x (listFreeTypeVars [] t)) ->
            let subst = unifyConstraints (substConstraints x t constraints)
            (x, substTypes subst t) :: subst
        | (FunBTy (s1, s2), FunBTy (t1, t2)) ->
            unifyConstraints ((s1, t1) :: (s2, t2) :: constraints)
        | (IntBTy, IntBTy) ->
            unifyConstraints constraints
        | (_, _) ->
            failwithf "failed to unify constraints: %A = %A" s t

let splitSchema (scm : BSchema) : list<Ident> * BType =
    let gensym = freshIdentGen "_T"
    let rec go scm =
        match scm with
        | BaseBScm t ->
            ([], t)
        | LamBScm (x, scm) ->
            let (acc, t) = go scm
            let y = gensym ()
            let t = substType x (VarBTy y) t
            (y :: acc, t)
    let (acc, t) = go scm
    (List.rev acc, t)

let listFreeTypeVarsOfSchema (scm : BSchema) : list<Ident> =
    let (env, t) = splitSchema scm
    listFreeTypeVars env t

// Hindley/Milner type inference
let inferTypes (f : UExpr) (annot : option<BSchema>) : BExpr * BSchema =
    let f = convertDeBruijnIndex f
    let (f, t, constraints) = listConstraints f
    let (tvars, constraints) =
        match annot with
        | None ->
            ([], constraints)
        | Some scm ->
            let (tvars, u) = splitSchema scm
            (tvars, (t, u) :: constraints)
    let subst = unifyConstraints constraints
    let f = substTypesOfExpr subst f
    let t = substTypes subst t
    let used = ref []
    let push scm x =
        match substTypes subst (VarBTy x) with
        | VarBTy (Ident x) when x.StartsWith "_" && not (List.contains (Ident x) (! used)) ->
            used := Ident x :: ! used
            LamBScm (Ident x, scm)
        | _ ->
            failwithf "failed to satisfy the type annotation: %A" annot
    let scm = List.fold push (BaseBScm t) tvars
    match listFreeTypeVarsOfSchema scm with
    | [] -> ()
    | tvars -> failwithf "the type schema is not closed: %A" tvars
    (f, scm)
