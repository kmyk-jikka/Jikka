module Jikka.Optimization

open Jikka.Semantics

let embed : list<ValName * Schema<RType>> =
    [ (ValName "+", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "-", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "*", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "/", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "%", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "**", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, NatRTy))))
      (ValName "negate", Monotype(FunRTy(ZahlRTy, ZahlRTy)))
      (ValName "max2", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
      (ValName "min2", Monotype(FunRTy(ZahlRTy, FunRTy(ZahlRTy, ZahlRTy))))
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
      (ValName "count3", Monotype(FunRTy(NatRTy, FunRTy(NatRTy, FunRTy(FunRTy(ZahlRTy, BoolRTy), NatRTy)))))
      (ValName "sum3", Monotype(FunRTy(NatRTy, FunRTy(NatRTy, FunRTy(FunRTy(ZahlRTy, ZahlRTy), ZahlRTy)))))
      (ValName "max3", Monotype(FunRTy(NatRTy, FunRTy(NatRTy, FunRTy(FunRTy(ZahlRTy, ZahlRTy), ZahlRTy)))))
      (ValName "min3", Monotype(FunRTy(NatRTy, FunRTy(NatRTy, FunRTy(FunRTy(ZahlRTy, ZahlRTy), ZahlRTy)))))
      (ValName "INT64_MAX", Monotype(ZahlRTy))
      (ValName "INT64_MIN", Monotype(ZahlRTy))
      (ValName "boolToZahl", Monotype(FunRTy(BoolRTy, ZahlRTy)))
      (ValName "zahlToBool", Monotype(FunRTy(ZahlRTy, BoolRTy))) ]

let foldBoundVar (f : 'a -> int -> int -> 'a) : 'a -> Expr -> 'a =
    let rec go depth acc =
        function
        | VarExp i -> f acc depth i
        | FreeVarExp _ -> acc
        | LamExp(_, e) -> go (depth + 1) acc e
        | AppExp(e1, e2) -> go depth (go depth acc e1) e2
        | FixpoExp(s, patterns, ts) -> List.fold (fun acc (_, e) -> go (depth + 1 + List.length ts) acc e) acc patterns
        | InductionExp(t1, t2, bases, step) -> go (depth + 2) (List.fold (fun acc e -> go depth acc e) acc bases) step
        | IfThenElseExp(e1, e2, e3) -> go depth (go depth (go depth acc e1) e2) e3
        | IntExp _ -> acc
        | BoolExp _ -> acc
    go 0

let mapBoundVar (f : int -> int -> Expr) : Expr -> Expr =
    let rec go depth e =
        match e with
        | VarExp i -> f depth i
        | FreeVarExp _ -> e
        | LamExp(t, e1) -> LamExp(t, go (depth + 1) e1)
        | AppExp(e1, e2) -> AppExp(go depth e1, go depth e2)
        | FixpoExp(s, patterns, ts) -> FixpoExp(s, List.map (fun (patterns, e) -> (patterns, go (depth + 1 + List.length ts) e)) patterns, ts)
        | InductionExp(t1, t2, bases, step) -> InductionExp(t1, t2, List.map (go depth) bases, go (depth + 2) step)
        | IfThenElseExp(e1, e2, e3) -> IfThenElseExp(go depth e1, go depth e2, go depth e3)
        | IntExp _ -> e
        | BoolExp _ -> e
    go 0

let findBoundVar (n : int) : Expr -> bool = foldBoundVar (fun acc depth i -> acc || i - depth = n) false
let hasAnyVar : Expr -> bool = foldBoundVar (fun _ _ _ -> true) false
let isClosedExpr : Expr -> bool = foldBoundVar (fun acc depth i -> acc && i - depth < 0) true

let unshiftLambda : Expr -> Expr =
    mapBoundVar (fun depth i ->
        VarExp(if i > depth then i - 1
               elif i < depth then i
               else failwith "failed to shift"))

let shiftLambda : Expr -> Expr =
    mapBoundVar (fun depth i ->
        VarExp(if i >= depth then i + 1
               else i))

let assignLambda (n : int) (e : Expr) : Expr -> Expr =
    mapBoundVar (fun depth i ->
        if i = n + depth then e
        else VarExp i)

type SplitIfThenElseRanges =
    { positive : list<Expr * Expr>
      negative : list<Expr * Expr>
      unknown : list<Expr * Expr * Expr>
      positive1 : list<Expr>
      negative1 : list<Expr>
      unknown1 : list<Expr * Expr> }

let emptyIfThenElseRanges : SplitIfThenElseRanges =
    { positive = []
      negative = []
      unknown = []
      positive1 = []
      negative1 = []
      unknown1 = [] }

let addIfThenElseRanges (a : SplitIfThenElseRanges) (b : SplitIfThenElseRanges) : SplitIfThenElseRanges =
    { positive = List.append a.positive b.positive
      negative = List.append a.negative b.negative
      unknown = List.append a.unknown b.unknown
      positive1 = List.append a.positive1 b.positive1
      negative1 = List.append a.negative1 b.negative1
      unknown1 = List.append a.unknown1 b.unknown1 }

let splitPredicateToRanges (fvar : ValName -> Expr) : Expr -> Expr -> Expr -> SplitIfThenElseRanges =
    let ranges = emptyIfThenElseRanges
    let fvar name = fvar (ValName name)
    let incr (x : Expr) : Expr = AppExp(AppExp(fvar "+", x), IntExp 1I)

    let rec go l r pred =
        match pred with
        | BoolExp true -> { ranges with positive = [ (l, r) ] }
        | BoolExp false -> { ranges with negative = [ (l, r) ] }
        | AppExp(AppExp(FreeVarExp(ValName name, _), a), b) ->
            let unary (a : Expr) : SplitIfThenElseRanges =
                match (name, a) with
                | ("=", a) ->
                    { ranges with positive1 = [ a ]
                                  negative =
                                      [ (l, a)
                                        (incr a, r) ] }
                | ("<>", a) ->
                    { ranges with positive =
                                      [ (l, a)
                                        (incr a, r) ]
                                  negative1 = [ a ] }
                | ("<", a) ->
                    { ranges with positive = [ (l, a) ]
                                  negative = [ (a, r) ] }
                | ("<=", a) ->
                    { ranges with positive = [ (l, incr a) ]
                                  negative = [ (incr a, r) ] }
                | (">=", a) ->
                    { ranges with positive = [ (a, r) ]
                                  negative = [ (l, a) ] }
                | (">", a) ->
                    { ranges with positive = [ (incr a, r) ]
                                  negative = [ (l, incr a) ] }
                | _ -> { ranges with unknown = [ (l, r, pred) ] }
            match (name, a, b) with
            | (_, VarExp 0, b) when not (findBoundVar 0 b) -> unary (unshiftLambda b)
            | (_, a, VarExp 0) when not (findBoundVar 0 a) -> unary (unshiftLambda a)
            | ("&&", a, b) -> { ranges with unknown = [ (l, r, pred) ] }
            | ("||", a, b) -> { ranges with unknown = [ (l, r, pred) ] }
            | _ -> { ranges with unknown = [ (l, r, pred) ] }
        | AppExp(FreeVarExp(ValName name, _), a) ->
            match (name, a) with
            | ("!", a) ->
                let ranges = go l r a
                { positive = ranges.positive
                  negative = ranges.negative
                  unknown = List.map (fun (l, r, pred) -> (l, r, AppExp(fvar "!", pred))) ranges.unknown
                  positive1 = ranges.positive1
                  negative1 = ranges.negative1
                  unknown1 = List.map (fun (k, pred) -> (k, AppExp(fvar "!", pred))) ranges.unknown1 }
            | _ -> { ranges with unknown = [ (l, r, pred) ] }
        | _ -> { ranges with unknown = [ (l, r, pred) ] }
    go

let isArray (stk : list<RType>) (e : Expr) : option<IntExpr * RType> =
    match getType stk e with
    | FunRTy(OrdinalRTy n, t) -> Some(n, t)
    | _ -> None

let rec toIntExpr : Expr -> option<IntExpr> =
    function
    | IntExp n -> Some(LiteralIExp n)
    | FreeVarExp(a, t) ->
        match t with
        | ZahlRTy -> Some(VarIExp a)
        | NatRTy -> Some(VarIExp a)
        | OrdinalRTy _ -> Some(VarIExp a)
        | RangeRTy _ -> Some(VarIExp a)
        | _ -> None
    | AppExp(FreeVarExp(ValName "negate", _), a) -> Option.map NegateIExp (toIntExpr a)
    | AppExp(AppExp(FreeVarExp(ValName f, _), a), b) ->
        let op a b =
            match f with
            | "+" -> Some(AddIExp(a, b))
            | "-" -> Some(AddIExp(a, b))
            | "*" -> Some(AddIExp(a, b))
            | "/" -> Some(AddIExp(a, b))
            | "%" -> Some(AddIExp(a, b))
            | "**" -> Some(AddIExp(a, b))
            | _ -> None
        Option.bind (fun a -> Option.bind (fun b -> op a b) (toIntExpr b)) (toIntExpr a)
    | _ -> None

let fromIntExpr (fvar : ValName -> Expr) : IntExpr -> Expr =
    let fvar s = fvar (ValName s)

    let rec go =
        function
        | LiteralIExp n -> IntExp n
        | VarIExp(ValName a) -> fvar a
        | NegateIExp e -> AppExp(fvar "negate", go e)
        | AddIExp(e1, e2) -> AppExp(AppExp(fvar "+", go e1), go e2)
        | SubIExp(e1, e2) -> AppExp(AppExp(fvar "-", go e1), go e2)
        | MulIExp(e1, e2) -> AppExp(AppExp(fvar "*", go e1), go e2)
        | DivIExp(e1, e2) -> AppExp(AppExp(fvar "/", go e1), go e2)
        | ModIExp(e1, e2) -> AppExp(AppExp(fvar "%", go e1), go e2)
        | PowIExp(e1, e2) -> AppExp(AppExp(fvar "**", go e1), go e2)
    go

let optimize (toplevel : ref<list<Defined>>) (gensym_t : unit -> TyName) (gensym_v : unit -> ValName) : Expr -> Expr =
    let app f x = AppExp(f, x)
    let app2 f x y = AppExp(AppExp(f, x), y)
    let app3 f x y z = AppExp(AppExp(AppExp(f, x), y), z)

    let fvarTuple (name : string) : ValName * RType =
        match List.tryFind (fun (x, _) -> x = ValName name) (getTypeEnv !toplevel) with
        | None -> failwithf "undefined name: %s" name
        | Some(x, scm) -> (x, realizeSchema gensym_t scm)

    let fvar (name : string) : Expr = FreeVarExp(fvarTuple name)
    let appf name x = AppExp(fvar name, x)
    let app2f name x y = AppExp(AppExp(fvar name, x), y)
    let app3f name x y z = AppExp(AppExp(AppExp(fvar name, x), y), z)
    let app4f name x y z w = AppExp(AppExp(AppExp(AppExp(fvar name, x), y), z), w)
    let add : Expr -> Expr -> Expr = app2f "+"
    let sub : Expr -> Expr -> Expr = app2f "-"
    let mul : Expr -> Expr -> Expr = app2f "*"
    let incr (x : Expr) : Expr = add x (IntExp 1I)
    let sum3 : Expr -> Expr -> Expr -> Expr = app3f "sum3"
    let addOrSub name = (name = "+" || name = "-")
    let maxOrMin name = (name = "max3" || name = "min3")
    let compareOp name = (name = "=" || name = "<>" || name = "<" || name = "<=" || name = ">" || name = ">=")
    let fvarFromValName (ValName name) : Expr = fvar name

    let smallOp =
        function // : 'b -> 'a -> 'b
        | "count" -> LamExp(VarRTy(gensym_t()), LamExp(VarRTy(gensym_t()), app2f "+" (VarExp 1) (appf "boolToZahl" (VarExp 0))))
        | "count3" -> LamExp(VarRTy(gensym_t()), LamExp(VarRTy(gensym_t()), app2f "+" (VarExp 1) (appf "boolToZahl" (VarExp 0))))
        | "sum" -> fvar "+"
        | "sum3" -> fvar "+"
        | "max" -> fvar "max2"
        | "max3" -> fvar "max2"
        | "min" -> fvar "min2"
        | "min3" -> fvar "min2"
        | name -> failwithf "not a big op: %s" name

    let unitOfSmallOp =
        function // : 'b
        | "count" -> IntExp 0I
        | "count3" -> IntExp 0I
        | "sum" -> IntExp 0I
        | "sum3" -> IntExp 0I
        | "max" -> fvar "INT64_MIN"
        | "max3" -> fvar "INT64_MIN"
        | "min" -> fvar "INT64_MAX"
        | "min3" -> fvar "INT64_MAX"
        | name -> failwithf "not a big op: %s" name

    let rec go (acc : list<RType>) : Expr -> (Expr * RType) =
        function
        | VarExp i -> (VarExp i, acc.[i])
        | FreeVarExp(ValName x, _) ->
            let (x, t) = fvarTuple x
            (FreeVarExp(x, t), t)
        | LamExp(_, AppExp(e, VarExp 0)) when not (findBoundVar 0 e) -> go acc (unshiftLambda e)
        | LamExp(t1, e) ->
            let (e, t2) = go (t1 :: acc) e
            (LamExp(t1, e), FunRTy(t1, t2))
        | AppExp(LamExp(t, e1), e2) ->
            let (e1, t1) = go (t :: acc) e1
            let (e2, t2) = go acc e2
            go acc (unshiftLambda (assignLambda 0 e2 e1))
        | AppExp(f, x) ->
            let (f, t) = go acc f

            let t =
                match t with
                | FunRTy(_, t) -> t
                | _ -> failwithf "failed to construct a type: %A %A %A" f x t

            let optimize e = fst (go [] e)

            let go e =
                let (e, _) = go acc e in e

            let y =
                match AppExp(f, x) with
                | AppExp(FreeVarExp(ValName f, t), x) ->
                    let x = go x
                    match (f, x) with
                    | ("negate", IntExp n) -> IntExp(-n)
                    | ("negate", AppExp(FreeVarExp(ValName "negate", _), e)) -> e
                    | ("!", BoolExp p) -> BoolExp(not p)
                    | ("!", AppExp(FreeVarExp(ValName "!", _), e)) -> e
                    | ("!", AppExp(AppExp(FreeVarExp(ValName "=", _), a), b)) -> go (app2f "<>" a b)
                    | ("!", AppExp(AppExp(FreeVarExp(ValName "<>", _), a), b)) -> go (app2f "=" a b)
                    | ("!", AppExp(AppExp(FreeVarExp(ValName "<", _), a), b)) -> go (app2f ">=" a b)
                    | ("!", AppExp(AppExp(FreeVarExp(ValName "<=", _), a), b)) -> go (app2f ">" a b)
                    | ("!", AppExp(AppExp(FreeVarExp(ValName ">=", _), a), b)) -> go (app2f "<" a b)
                    | ("!", AppExp(AppExp(FreeVarExp(ValName ">", _), a), b)) -> go (app2f "<=" a b)
                    | _ -> AppExp(FreeVarExp(ValName f, t), x)
                | AppExp(AppExp(FreeVarExp(ValName f, t), x), y) ->
                    let x = go x
                    let y = go y
                    match (f, x, y) with
                    | ("+", IntExp a, IntExp b) -> IntExp(a + b)
                    | ("-", IntExp a, IntExp b) -> IntExp(a - b)
                    | ("*", IntExp a, IntExp b) -> IntExp(a * b)
                    | ("/", IntExp a, IntExp b) -> IntExp(a / b)
                    | ("%", IntExp a, IntExp b) -> IntExp(a % b)
                    | ("**", IntExp a, IntExp b) -> IntExp(a ** int (b))
                    | ("+", e, IntExp n) when n = 0I -> e
                    | ("-", e, IntExp n) when n = 0I -> e
                    | ("*", e, IntExp n) when n = 1I -> e
                    | ("*", _, IntExp n) when n = 0I -> IntExp 0I
                    | ("*", e, IntExp n) when n = -1I -> go (app (fvar "negate") e)
                    | ("/", e, IntExp n) when n = 1I -> e
                    | ("/", e, IntExp n) when n = -1I -> go (app (fvar "negate") e)
                    | ("%", _, IntExp n) when n = 1I -> IntExp 0I
                    | ("**", e, IntExp n) when n = 1I -> e
                    | ("+", IntExp n, e) when n = 0I -> e
                    | ("-", IntExp n, e) when n = 0I -> go (app (fvar "negate") e)
                    | ("*", IntExp n, e) when n = 1I -> e
                    | ("*", IntExp n, _) when n = 0I -> IntExp 0I
                    | ("*", IntExp n, e) when n = -1I -> go (app (fvar "negate") e)
                    | ("/", IntExp n, _) when n = 0I -> IntExp 0I
                    | ("/", IntExp n, e) when n = -1I -> go (app (fvar "negate") e)
                    | ("%", IntExp n, _) when n = 0I -> IntExp 0I
                    | ("**", IntExp n, _) when n = 1I -> IntExp 1I
                    | ("&&", BoolExp a, BoolExp b) -> BoolExp(a && b)
                    | ("||", BoolExp a, BoolExp b) -> BoolExp(a || b)
                    | ("&&", BoolExp true, e) -> e
                    | ("&&", BoolExp false, _) -> BoolExp false
                    | ("||", BoolExp true, _) -> BoolExp true
                    | ("||", BoolExp false, e) -> e
                    | ("&&", e, BoolExp true) -> e
                    | ("&&", _, BoolExp false) -> BoolExp false
                    | ("||", _, BoolExp true) -> BoolExp true
                    | ("||", e, BoolExp false) -> e
                    | ("<", IntExp a, IntExp b) -> BoolExp(a < b)
                    | ("<=", IntExp a, IntExp b) -> BoolExp(a <= b)
                    | ("=", IntExp a, IntExp b) -> BoolExp(a = b)
                    | ("<>", IntExp a, IntExp b) -> BoolExp(a <> b)
                    | (">=", IntExp a, IntExp b) -> BoolExp(a >= b)
                    | (">", IntExp a, IntExp b) -> BoolExp(a > b)
                    | ("<", a, b) when a = b -> BoolExp false
                    | ("<=", a, b) when a = b -> BoolExp true
                    | ("=", a, b) when a = b -> BoolExp true
                    | ("<>", a, b) when a = b -> BoolExp false
                    | (">=", a, b) when a = b -> BoolExp true
                    | (">", a, b) when a = b -> BoolExp false
                    | ("=", e, IntExp n) when n = 0I -> go (appf "!" (appf "zahlToBool" e))
                    | ("=", IntExp n, e) when n = 0I -> go (appf "!" (appf "zahlToBool" e))
                    | ("<>", e, IntExp n) when n = 0I -> go (appf "zahlToBool" e)
                    | ("<>", IntExp n, e) when n = 0I -> go (appf "zahlToBool" e)
                    | (">=", a, b) -> go (app2f "<=" b a)
                    | (">", a, b) -> go (app2f "<" b a)
                    | ("<", a, AppExp(AppExp(FreeVarExp(ValName "+", _), IntExp n), b)) when n = 1I -> go (app2f "<=" a b)
                    | (cmp, AppExp(AppExp(FreeVarExp(ValName "+", _), IntExp n1), a), AppExp(AppExp(FreeVarExp(ValName "+", _), IntExp n2), b)) when compareOp cmp && n1 = n2 -> go (app2f cmp a b)
                    | ("count", n, f) -> go (app3f "count3" (IntExp 0I) n f)
                    | ("sum", n, f) -> go (app3f "sum3" (IntExp 0I) n f)
                    | ("max", n, f) -> go (app3f "max3" (IntExp 0I) n f)
                    | ("min", n, f) -> go (app3f "min3" (IntExp 0I) n f)
                    | _ -> AppExp(AppExp(FreeVarExp(ValName f, t), x), y)
                | AppExp(AppExp(AppExp(FreeVarExp(ValName f, t), x), y), z) ->
                    let x = go x
                    let y = go y
                    let z = go z
                    match (f, x, y, z) with
                    | ("count3", IntExp l, IntExp r, _) when l = r -> IntExp 0I
                    | ("count3", l, r, LamExp(_, e)) when not (findBoundVar 0 e) -> go (mul (sub r l) (appf "boolToZahl" (unshiftLambda e)))
                    | ("sum3", IntExp l, IntExp r, _) when l = r -> IntExp 0I
                    | ("sum3", l, r, LamExp(_, e)) when not (findBoundVar 0 e) -> go (mul (sub r l) (unshiftLambda e))
                    | ("sum3", l, r, LamExp(t, AppExp(AppExp(FreeVarExp(ValName op, _), e1), e2))) when addOrSub op ->
                        let e1 = sum3 l r (LamExp(t, e1))
                        let e2 = sum3 l r (LamExp(t, e2))
                        go (app2f op e1 e2)
                    | ("sum3", l, r, LamExp(t, AppExp(AppExp(FreeVarExp(ValName "*", _), e1), e2))) when not (findBoundVar 0 e1) ->
                        let e1 = unshiftLambda e1
                        let e2 = sum3 l r (LamExp(t, e2))
                        go (mul e1 e2)
                    | ("sum3", l, r, LamExp(t, AppExp(AppExp(FreeVarExp(ValName "*", _), e1), e2))) when not (findBoundVar 0 e2) ->
                        let e1 = sum3 l r (LamExp(t, e1))
                        let e2 = unshiftLambda e2
                        go (mul e1 e2)
                    | (maxlike, _, _, LamExp(t, e)) when maxOrMin maxlike && not (findBoundVar 0 e) -> unshiftLambda e
                    | (maxlike, l, r, LamExp(t, AppExp(AppExp(FreeVarExp(ValName op, _), e1), e2))) when maxOrMin maxlike && addOrSub op && not (findBoundVar 0 e1) ->
                        let e1 = unshiftLambda e1
                        let e2 = app3f maxlike l r (LamExp(t, e2))
                        go (app2f op e1 e2)
                    | (maxlike, l, r, LamExp(t, AppExp(AppExp(FreeVarExp(ValName op, _), e1), e2))) when maxOrMin maxlike && addOrSub op && not (findBoundVar 0 e2) ->
                        let e1 = app3f maxlike l r (LamExp(t, e1))
                        let e2 = unshiftLambda e2
                        go (app2f op e1 e2)
                    | (op, l, r, LamExp(t, IfThenElseExp(e1, e2, e3))) when op = "count3" || op = "sum3" || maxOrMin op ->
                        let (plus, zero) =
                            match op with
                            | "count3" -> (fvar "+", IntExp 0I)
                            | "sum3" -> (fvar "+", IntExp 0I)
                            | "max3" -> (fvar "max2", fvar "INT64_MIN")
                            | "min3" -> (fvar "min2", fvar "INT64_MAX")
                            | _ -> failwithf "something wrong: %s" op

                        let lam e = LamExp(t, e)
                        let ranges = splitPredicateToRanges fvarFromValName l r e1
                        let e = ref zero
                        for (l, r) in ranges.positive do
                            e := app2 plus !e (app3f op l r (lam e2))
                        for (l, r) in ranges.negative do
                            e := app2 plus !e (app3f op l r (lam e3))
                        for (l, r, pred) in ranges.unknown do
                            e := app2 plus !e (app3f op l r (lam (IfThenElseExp(pred, e2, e3))))
                        for k in ranges.positive1 do
                            e := app2 plus !e (unshiftLambda (assignLambda 0 k e2))
                        for k in ranges.negative1 do
                            e := app2 plus !e (unshiftLambda (assignLambda 0 k e3))
                        for (k, pred) in ranges.unknown1 do
                            e := app2 plus !e (unshiftLambda (assignLambda 0 k (IfThenElseExp(pred, e2, e3))))
                        go !e
                    | (op, l, r, e) when (op = "count3" || op = "sum3") && Option.isSome (isArray acc e) && isClosedExpr e ->
                        let (n, t) = Option.get (isArray acc e)
                        let name = gensym_v()
                        let base_ = IntExp 0I
                        let step = add (app (VarExp 1) (VarExp 0)) (app (shiftLambda (shiftLambda e)) (VarExp 0))
                        let def = InductionExp(OrdinalRTy(AddIExp(n, LiteralIExp 1I)), t, [ base_ ], step)
                        let (def, scm) = inferTypes gensym_t def None
                        let def = optimize def
                        let (def, scm) = inferTypes gensym_t def None
                        toplevel := Defined(name, def, scm) :: !toplevel
                        let e = fvarFromValName name
                        go (sub (app e r) (app e l))
                    | (op, l, r, e) when maxOrMin op && isClosedExpr e && isClosedExpr l && isClosedExpr r ->
                        let name = gensym_v()
                        let (e, scm) = inferTypes gensym_t (app3f op l r e) None
                        toplevel := Defined(name, e, scm) :: !toplevel
                        fvarFromValName name
                    | (op, l, r, e) when maxOrMin op && isClosedExpr e && Option.isSome (isArray acc e) && isClosedExpr l ->
                        eprintfn "LLL %A %A %A" l r e
                        // vector<int> acc(n + 1);
                        // REP (i, n) {
                        //     if (i <= l || r < i) {
                        //         acc[i] = 0;
                        //     } else {
                        //         acc[i + 1] = acc[i] + ...;
                        //     }
                        // }
                        let (n, t) = Option.get (isArray acc e)
                        let name = gensym_v()
                        let base_ = unitOfSmallOp op

                        let step =
                            let acc = app (VarExp 1) (VarExp 0)
                            let i = VarExp 0
                            let if_ = app2f "<=" l i
                            let then_ = app2 (smallOp op) acc (app (shiftLambda (shiftLambda e)) i)
                            let else_ = unitOfSmallOp op
                            IfThenElseExp(if_, then_, else_)

                        let def = InductionExp(OrdinalRTy(AddIExp(n, LiteralIExp 1I)), t, [ base_ ], step)
                        eprintfn "%A" def
                        let (def, scm) = inferTypes gensym_t def None
                        let def = optimize def
                        let (def, scm) = inferTypes gensym_t def None
                        toplevel := Defined(name, def, scm) :: !toplevel
                        let e = fvarFromValName name
                        go (app e r)
                    | (op, l, r, e) when maxOrMin op && isClosedExpr e && Option.isSome (isArray acc e) && isClosedExpr r ->
                        eprintfn "RRR %A %A %A" l r e
                        let (n, t) = Option.get (isArray acc e)
                        let rev e = sub (sub (fromIntExpr fvarFromValName n) e) (IntExp 1I)
                        let name = gensym_v()
                        let base_ = unitOfSmallOp op

                        let step =
                            let acc = app (VarExp 1) (VarExp 0)
                            let i = rev (VarExp 0)
                            let if_ = app2f "<" i r
                            let then_ = app2 (smallOp op) acc (app (shiftLambda (shiftLambda e)) i)
                            let else_ = unitOfSmallOp op
                            IfThenElseExp(if_, then_, else_)

                        let def = InductionExp(OrdinalRTy(AddIExp(n, LiteralIExp 1I)), t, [ base_ ], step)
                        let (def, scm) = inferTypes gensym_t def None
                        let def = optimize def
                        let (def, scm) = inferTypes gensym_t def None
                        toplevel := Defined(name, def, scm) :: !toplevel
                        let e = fvarFromValName name
                        go (app e (add (rev l) (IntExp 1I)))
                    | _ -> AppExp(AppExp(AppExp(FreeVarExp(ValName f, t), x), y), z)
                | _ -> AppExp(f, go x)

            (y, t)
        | FixpoExp(s, patterns, ts) ->
            let f (patterns, e) =
                let (e, _) = go (List.append (List.rev ts) (s :: acc)) e
                (patterns, e)

            let patterns = List.map f patterns
            let e = FixpoExp(s, patterns, ts)
            (e, s)
        | InductionExp(t1, t2, bases, step) ->
            let bases = List.map (fun e -> fst (go acc e)) bases
            let (step, _) = go (t1 :: FunRTy(t1, t2) :: acc) step
            (InductionExp(t1, t2, bases, step), FunRTy(t1, t2))
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
