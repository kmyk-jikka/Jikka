module Optimization

open Semantics

let embed : list<ValName * Schema<RType>> =
    [ (ValName "+", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "-", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "*", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "/", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "%", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "**", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, NatRTy))))
      (ValName "negate", Monotype(FunRTy(ZahlRTy, ZahlRTy)))
      (ValName "!", Monotype(FunRTy(BoolRTy, BoolRTy)))
      (ValName "&&", Monotype(FunRTy(BoolRTy, FunRTy(BoolRTy, BoolRTy))))
      (ValName "||", Monotype(FunRTy(BoolRTy, FunRTy(BoolRTy, BoolRTy))))
      (ValName "<=", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, BoolRTy))))
      (ValName "<", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, BoolRTy))))
      (ValName "=", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, BoolRTy))))
      (ValName "<>", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, BoolRTy))))
      (ValName ">=", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, BoolRTy))))
      (ValName ">", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, BoolRTy))))
      (ValName "count", Monotype(FunRTy(NatRTy, FunRTy(FunRTy(ZahlRTy, BoolRTy), NatRTy))))
      (ValName "sum", Monotype(FunRTy(NatRTy, FunRTy(FunRTy(ZahlRTy, ZahlRTy), ZahlRTy))))
      (ValName "max", Monotype(FunRTy(NatRTy, FunRTy(FunRTy(ZahlRTy, ZahlRTy), ZahlRTy))))
      (ValName "min", Monotype(FunRTy(NatRTy, FunRTy(FunRTy(ZahlRTy, ZahlRTy), ZahlRTy))))
      (ValName "boolToZahl", Monotype(FunRTy(BoolRTy, ZahlRTy)))
      (ValName "zahlToBool", Monotype(FunRTy(ZahlRTy, BoolRTy))) ]

let foldBoundVar (f : 'a -> int -> 'a) : 'a -> Expr -> 'a =
    let rec go depth acc =
        function
        | VarExp i -> f acc (i - depth)
        | FreeVarExp _ -> acc
        | LamExp(_, e) -> go (depth + 1) acc e
        | AppExp(e1, e2) -> go depth (go depth acc e1) e2
        | IfThenElseExp(e1, e2, e3) -> go depth (go depth (go depth acc e1) e2) e3
        | IntExp _ -> acc
        | BoolExp _ -> acc
    go 0

let mapBoundVar (f : int -> Expr) : Expr -> Expr =
    let rec go depth e =
        match e with
        | VarExp i -> f (i - depth)
        | FreeVarExp _ -> e
        | LamExp(t, e1) -> LamExp(t, go (depth + 1) e1)
        | AppExp(e1, e2) -> AppExp(go depth e1, go depth e2)
        | IfThenElseExp(e1, e2, e3) -> IfThenElseExp(go depth e1, go depth e2, go depth e3)
        | IntExp _ -> e
        | BoolExp _ -> e
    go 0

let findBoundVar (n : int) : Expr -> bool = foldBoundVar (fun acc i -> acc || i = n) false

let unshiftLambda : Expr -> Expr =
    mapBoundVar (fun i ->
        VarExp(if i > 0 then i - 1
               elif i < 0 then i
               else failwith "failed to shift"))

let assignLambda (n : int) (e : Expr) : Expr -> Expr =
    mapBoundVar (fun i ->
        if i = n then e
        else VarExp i)

let optimize (gensym : unit -> TyName) : Expr -> Expr =
    let app f x = AppExp(f, x)
    let app2 f x y = AppExp(AppExp(f, x), y)

    let fvar (name : string) : Expr =
        match List.tryFind (fun (x, _) -> x = ValName name) embed with
        | None -> failwithf "undefined name: %s" name
        | Some(x, scm) -> FreeVarExp(x, realizeSchema gensym scm)

    let appf name x = AppExp(fvar name, x)
    let add : Expr -> Expr -> Expr = app2 (fvar "+")
    let sub : Expr -> Expr -> Expr = app2 (fvar "-")
    let mul : Expr -> Expr -> Expr = app2 (fvar "*")

    let rec go (acc : list<RType>) : Expr -> (Expr * RType) =
        function
        | VarExp i -> (VarExp i, acc.[i])
        | FreeVarExp(x, t) -> (FreeVarExp(x, t), t)
        | LamExp(t1, e) ->
            let (e, t2) = go (t1 :: acc) e
            (LamExp(t1, e), FunRTy(t1, t2))
        | AppExp(f, x) ->
            let (f, t1) = go acc f
            let (x, t2) = go acc x

            let y =
                let go e =
                    let (e, _) = go acc e in e
                match (f, x) with
                | (AppExp(FreeVarExp(ValName "add", _), IntExp a), IntExp b) -> IntExp(a + b)
                | (AppExp(FreeVarExp(ValName "sub", _), IntExp a), IntExp b) -> IntExp(a - b)
                | (AppExp(FreeVarExp(ValName "mul", _), IntExp a), IntExp b) -> IntExp(a * b)
                | (AppExp(FreeVarExp(ValName "div", _), IntExp a), IntExp b) -> IntExp(a / b)
                | (AppExp(FreeVarExp(ValName "mod", _), IntExp a), IntExp b) -> IntExp(a % b)
                | (AppExp(FreeVarExp(ValName "pow", _), IntExp a), IntExp b) -> IntExp(a ** int (b))
                | (AppExp(FreeVarExp(ValName "add", _), e), IntExp n) when n = 0I -> e
                | (AppExp(FreeVarExp(ValName "sub", _), e), IntExp n) when n = 0I -> e
                | (AppExp(FreeVarExp(ValName "mul", _), e), IntExp n) when n = 1I -> e
                | (AppExp(FreeVarExp(ValName "mul", _), _), IntExp n) when n = 0I -> IntExp 0I
                | (AppExp(FreeVarExp(ValName "mul", _), e), IntExp n) when n = -1I -> go (app (fvar "negate") e)
                | (AppExp(FreeVarExp(ValName "div", _), e), IntExp n) when n = 1I -> e
                | (AppExp(FreeVarExp(ValName "div", _), e), IntExp n) when n = -1I -> go (app (fvar "negate") e)
                | (AppExp(FreeVarExp(ValName "mod", _), _), IntExp n) when n = 1I -> IntExp 0I
                | (AppExp(FreeVarExp(ValName "pow", _), e), IntExp n) when n = 1I -> e
                | (AppExp(FreeVarExp(ValName "add", _), IntExp n), e) when n = 0I -> e
                | (AppExp(FreeVarExp(ValName "sub", _), IntExp n), e) when n = 0I -> go (app (fvar "negate") e)
                | (AppExp(FreeVarExp(ValName "mul", _), IntExp n), e) when n = 1I -> e
                | (AppExp(FreeVarExp(ValName "mul", _), IntExp n), _) when n = 0I -> IntExp 0I
                | (AppExp(FreeVarExp(ValName "mul", _), IntExp n), e) when n = -1I -> go (app (fvar "negate") e)
                | (AppExp(FreeVarExp(ValName "div", _), IntExp n), _) when n = 0I -> IntExp 0I
                | (AppExp(FreeVarExp(ValName "div", _), IntExp n), e) when n = -1I -> go (app (fvar "negate") e)
                | (AppExp(FreeVarExp(ValName "mod", _), IntExp n), _) when n = 0I -> IntExp 0I
                | (AppExp(FreeVarExp(ValName "pow", _), IntExp n), _) when n = 1I -> IntExp 1I
                | (FreeVarExp(ValName "negate", _), IntExp n) -> IntExp(-n)
                | (FreeVarExp(ValName "negate", _), AppExp(FreeVarExp(ValName "negate", _), e)) -> e
                | (FreeVarExp(ValName "!", _), BoolExp p) -> BoolExp(not p)
                | (FreeVarExp(ValName "!", _), AppExp(FreeVarExp(ValName "!", _), e)) -> e
                | (AppExp(FreeVarExp(ValName "&&", _), BoolExp a), BoolExp b) -> BoolExp(a && b)
                | (AppExp(FreeVarExp(ValName "||", _), BoolExp a), BoolExp b) -> BoolExp(a || b)
                | (AppExp(FreeVarExp(ValName "&&", _), BoolExp true), e) -> e
                | (AppExp(FreeVarExp(ValName "&&", _), BoolExp false), _) -> BoolExp false
                | (AppExp(FreeVarExp(ValName "||", _), BoolExp true), _) -> BoolExp true
                | (AppExp(FreeVarExp(ValName "||", _), BoolExp false), e) -> e
                | (AppExp(FreeVarExp(ValName "&&", _), e), BoolExp true) -> e
                | (AppExp(FreeVarExp(ValName "&&", _), _), BoolExp false) -> BoolExp false
                | (AppExp(FreeVarExp(ValName "||", _), _), BoolExp true) -> BoolExp true
                | (AppExp(FreeVarExp(ValName "||", _), e), BoolExp false) -> e
                | (AppExp(FreeVarExp(ValName "<", _), IntExp a), IntExp b) -> BoolExp(a < b)
                | (AppExp(FreeVarExp(ValName "<=", _), IntExp a), IntExp b) -> BoolExp(a <= b)
                | (AppExp(FreeVarExp(ValName "=", _), IntExp a), IntExp b) -> BoolExp(a = b)
                | (AppExp(FreeVarExp(ValName "<>", _), IntExp a), IntExp b) -> BoolExp(a <> b)
                | (AppExp(FreeVarExp(ValName ">=", _), IntExp a), IntExp b) -> BoolExp(a >= b)
                | (AppExp(FreeVarExp(ValName ">", _), IntExp a), IntExp b) -> BoolExp(a > b)
                | (AppExp(FreeVarExp(ValName "=", _), e), IntExp n) when n = 0I -> go (appf "!" (appf "zahlToBool" e))
                | (AppExp(FreeVarExp(ValName "=", _), IntExp n), e) when n = 0I -> go (appf "!" (appf "zahlToBool" e))
                | (AppExp(FreeVarExp(ValName "<>", _), e), IntExp n) when n = 0I -> go (appf "zahlToBool" e)
                | (AppExp(FreeVarExp(ValName "<>", _), IntExp n), e) when n = 0I -> go (appf "zahlToBool" e)
                | (AppExp(FreeVarExp(ValName "count", _), IntExp n), _) when n = 0I -> IntExp 0I
                | (AppExp(FreeVarExp(ValName "count", _), n), LamExp(_, e)) when not (findBoundVar 0 e) -> go (mul n (appf "boolToZahl" (unshiftLambda e)))
                | (AppExp(FreeVarExp(ValName "sum", _), IntExp n), _) when n = 0I -> IntExp 0I
                | (AppExp(FreeVarExp(ValName "sum", _), n), LamExp(_, e)) when not (findBoundVar 0 e) -> go (mul n (unshiftLambda e))
                | (AppExp(FreeVarExp(ValName "sum", _), n), LamExp(t, AppExp(AppExp(FreeVarExp(ValName "+", _), e1), e2))) ->
                    let e1 = app2 (fvar "sum") n (LamExp(t, e1))
                    let e2 = app2 (fvar "sum") n (LamExp(t, e2))
                    go (add e1 e2)
                | _ -> AppExp(f, x)
            match t1 with
            | FunRTy(_, t3) -> (y, t3)
            | _ -> failwith "failed to construct a type"
        | IfThenElseExp(e1, e2, e3) ->
            let (e1, _) = go acc e1
            let (e2, t2) = go acc e2
            let (e3, t3) = go acc e3
            match e1 with
            | BoolExp p ->
                if p then (e2, t2)
                else (e3, t3)
            | _ -> (IfThenElseExp(e1, e2, e3), t2)
        | IntExp n -> (IntExp n, ZahlRTy)
        | BoolExp p -> (BoolExp p, BoolRTy)
    fun e ->
        let (e, _) = go [] e
        e
