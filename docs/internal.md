# docs/internal.md

(このドキュメントの日本語バージョン: [docs/internal.ja.md](https://github.com/kmyk/Jikka/blob/master/docs/internal.ja.md))

This document describes the internal implementation of Jikka.

## 0. Overview

The general structure of Jikka's internals is to perform the followings in order:

1.  Read a Python code
2.  Get a Python AST with parsing the Python code
3.  Convert the Python AST to an AST of our restricted Python
4.  Preprocess the AST of our restricted Python
5.  Convert the AST of a restricted Python to a AST of our core language
6.  Optimize the AST of our core language
7.  Convert the AST of our core languagee to a C++ AST
8.  Postprocess the C++ AST
9.  Convert the C++ AST to a C++ code
10. Write the C++ code

Jikka converts (standard) Python, our restricted Python, our core language and C++ in this order.
Here, our restricted Python is the language specified in [docs/language.md](https://github.com/kmyk/Jikka/blob/master/docs/language.md).
Our core language is a language which is similar to Haskell and is almost the same to [GHC Core](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/core-syn-type) which is the intermediate language of GHC the Haskell compiler.
This core language is described in [docs/core.md](https://github.com/kmyk/Jikka/blob/master/docs/language.md).

- List of modules [Jikka](https://kmyk.github.io/Jikka/)
- File: [src/Jikka/Main/Subcommand/Convert.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Main/Subcommand/Convert.hs) ([Jikka.Main.Subcommand.Convert](https://kmyk.github.io/Jikka/haddock/Jikka-Main-Subcommand-Convert.html))

## 2. Get a Python AST with parsing the Python code

After reading a Python code string, Jikka parses it based on [the grammar specification of Python](https://docs.python.org/ja/3/reference/grammar.html).
We use [lex](https://ja.wikipedia.org/wiki/Lex) (Its Haskell version [alex](https://www.haskell.org/alex/)) and [yacc](https://ja.wikipedia.org/wiki/Yacc) (Its Haskell version [happy](https://www.haskell.org/happy/)) to generate an LALR(1) parser.

- File: lex [src/Jikka/Python/Parse/Happy.y](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Python/Parse/Happy.y) ([Jikka.Python.Parse.Happy](https://kmyk.github.io/Jikka/haddock/Jikka-Python-Parse-Alex.html))
- File: yacc [src/Jikka/Python/Parse/Alex.x](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Python/Parse/Alex.x) ([Jikka.Python.Parse.Alex](https://kmyk.github.io/Jikka/haddock/Jikka-Python-Parse-Happy.html))
- Reference: [Modern Compiler Implement in ML](https://www.amazon.co.jp/dp/0521607647)

### Example

For example, consider the following Python code:

```python
def f(a, b) -> int:
    return a + b
```

From this code, we get the following syntax tree.
You can obtain this with running a command `$ python3 -c 'import ast; print(ast.dump(ast.parse("def f(a, b) -> int: return a + b")))'`.

```python
Module(
    body=[
        FunctionDef(
            name='f',
            args=arguments(
                posonlyargs=[],
                args=[
                    arg(arg='a', annotation=None, type_comment=None),
                    arg(arg='b', annotation=None, type_comment=None)
                ],
                vararg=None,
                kwonlyargs=[],
                kw_defaults=[],
                kwarg=None,
                defaults=[]
                ),
            body=[
                Return(
                    value=BinOp(
                        left=Name(id='a', ctx=Load()),
                        op=Add(),
                        right=Name(id='b', ctx=Load()))
                )
            ],
            decorator_list=[],
            returns=Name(id='int', ctx=Load()),
            type_comment=None
        )
    ],
    type_ignores=[])
```

## 3. Convert the Python AST to an AST of our restricted Python

Jikka has the complete AST same to the one of [`ast` module](https://docs.python.org/ja/3/library/ast.html) ([data Expr](https://hackage.haskell.org/package/Jikka/docs/Jikka-Python-Language-Expr.html#t:Expr)) after parsing Python.
Then, it removes unnecessary parts from this AST and converts it to a convenient AST for our restricted Python ([data Expr](https://hackage.haskell.org/package/Jikka/docs/Jikka-RestrictedPython-Language-Expr.html#t:Expr)).

- File: [src/Jikka/Python/Convert/ToRestrictedPython.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Python/Convert/ToRestrictedPython.hs) ([Jikka.Python.Convert.ToRestrictedPython](https://kmyk.github.io/Jikka/haddock/Jikka-Python-Convert-ToRestrictedPython.html))

## 4. Preprocess the AST of our restricted Python

Jikka performes the following preprocesses on the AST of our restricted Python:

1.  Checking and renaming variable names
1.  [Type inference](https://en.wikipedia.org/wiki/Type_inference)
1.  Other miscellaneous checking

It uses Hindley/Milner type inference algorithm.
This algorithm reconstructs types with collecting equations about type variables and solving them.

- File: [src/Jikka/RestrictedPython/Convert.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/RestrictedPython/Convert.hs) ([Jikka.RestrictedPython.Convert](https://kmyk.github.io/Jikka/haddock/Jikka-RestrictedPython-Convert.html))
- File: Checking and renaming variable names [src/Jikka/RestrictedPython/Convert/Alpha.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/RestrictedPython/Convert/Alpha.hs) ([Jikka.RestrictedPython.Convert.Alpha](https://kmyk.github.io/Jikka/haddock/Jikka-RestrictedPython-Convert-Alpha.html))
- File: Type inference [src/Jikka/RestrictedPython/Convert/TypeInfer.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/RestrictedPython/Convert/TypeInfer.hs) ([Jikka.RestrictedPython.Convert.TypeInfer](https://kmyk.github.io/Jikka/haddock/Jikka-RestrictedPython-Convert-TypeInfer.html))
- Reference: [Types and Programming Languages](https://www.amazon.co.jp/dp/0262162091)

## 5. Convert the AST of a restricted Python to a AST of our core language

It converts an AST of our restricted Python into an AST of the core language.

For example, Python has assignment statements and `for` loops, whereas the core language (Haskell) does not.
Therefore, all assignment statements are converted to `let` statements and `for` loops are converted to [foldl](https://hackage.haskell.org/package/base/docs/Prelude.html#v:foldl).

For example, consider an AST from the following Python code:

```python
def solve(n: int) -> int:
    a = 0
    b = 1
    for _ in range(n):
        c = a + b
        a = b
        b = c
    return a
```

This becomes an AST which corresponds to the following Haskell code:

```haskell
solve :: Int -> Int
solve n =
    let a0 = 0
    in let b0 = 1
    in let (a3, b3) =
             foldl (\(a1, b1) _ ->
                 let c = a1 + b1
                 in let a2 = b1
                 in let b2 = c
                 in (a2, b2)
               ) (a0, b0) [0..n - 1]
    in a3
```

- File: [src/Jikka/RestrictedPython/Convert/ToCore.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/RestrictedPython/Convert/ToCore.hs) ([Jikka.RestrictedPython.Convert.ToCore](https://kmyk.github.io/Jikka/haddock/Jikka-RestrictedPython-Convert-ToCore.html))

## 6. Optimize the AST of our core language

This is the main process of optimizations that Jikka does.
Jikka tries every optimizations we can think of.
Most of them are implemented as [rewrite rules](https://wiki.haskell.org/GHC/Using_rules).

At the moment, optimizations are done in a greedy way, which is looking for possible conversions using rewrite rules and always converts them.
In other words, Jikka doesn't perform searching such as DFS or a beam search.
Doing such complex optimizations are a future task.

- File: [src/Jikka/Core/Convert.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert.hs) ([Jikka.Core.Convert](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Convert.html))
- Directory: [src/Jikka/Core/Convert/](https://github.com/kmyk/Jikka/tree/master/src/Jikka/Core/Convert)

### Example: Cumulative Sum

For example, consider the following O(N²) Python code:

```python
def solve(n: int, a: List[int]) -> int:
    b = 0
    for i in range(n):
        b += sum(a[:i])
    return b
```

Before the optimization step, this Python code is already converted to the following Haskell code:

```haskell
solve :: Int -> [Int] -> Int
solve n a =
    foldl (\b i ->
        b + sum (map (\j -> a !! j) [0..i - 1])
      ) 0 [0..n - 1]
```

At first, a rewrite rule about cumulative sum "replace a sub-expression like `sum (map (\i -> xs !! i) [0..k - 1])` with an expresssion `let ys = scanl (+) 0 xs in ys !! k`" works, and the above code becomes the following code with [scanl](https://hackage.haskell.org/package/base/docs/Prelude.html#v:scanl):

```haskell
solve :: Int -> [Int] -> Int
solve n a =
    foldl (\b i ->
        let c = scanl (+) 0 a
        in b + c !! i
      ) 0 [0..n - 1]
```

Then a rewrite rule about [foldl](https://hackage.haskell.org/package/base/docs/Prelude.html#v:foldl) and `let` expression "if variables `y` and `x` are not used in a expression `c`, and if a variable `a` is not used in expressions `y0` and `xs`, then replace a sub-expression `foldl (\y x -> let a = c in e) y0 xs` with an expression `let a = c in foldl (\y x -> e) y0 xs`" works. The code becomes the following:

```haskell
solve :: Int -> [Int] -> Int
solve n a =
    let c = scanl (+) 0 a
    in foldl (\b i ->
           b + c !! i
         ) 0 [0..n - 1]
```

This result Haskell code will the following C++ code with the following steps.
This is O(N).

```c++
int solve(int n, vector<int> a) {
    vector<int> c;
    c.push_back(0);
    for (int i = 0; i < a.size(); ++ i) {
        c.push_back(c[i] + a[i]);
    }
    int b = 0;
    for (int i = 0; i < n; ++ i) {
        b += c[i];
    }
    return b;
}
```

- File: [src/Jikka/Core/Convert/CumulativeSum.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/CumulativeSum.hs) ([Jikka.Core.Convert.CumulativeSum](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Convert-CumulativeSum.html))
- File: [src/Jikka/Core/Convert/BubbleLet.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/BubbleLet.hs) ([Jikka.Core.Convert.BubbleLet](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Convert-BubbleLet.html))

### Example of Implementation: Short Cut Fusion

Let's see the implementation of module [Jikka.Core.Convert.ShortCutFusion](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Convert-ShortCutFusion.html) for [Short cut fusion](https://wiki.haskell.org/Short_cut_fusion).
For example, rewrite rules `reduceFoldBuild` is defined as follows [at `v5.1.0.0`](https://github.com/kmyk/Jikka/blob/795726a626ca3653555f6c5c176eb81de26b6d58/src/Jikka/Core/Convert/ShortCutFusion.hs#L162-L183):

```haskell
reduceFoldBuild :: MonadAlpha m => RewriteRule m
reduceFoldBuild =
  let return' = return . Just
   in RewriteRule $ \_ -> \case
        -- reduce `Foldl`
        Foldl' _ _ _ init (Nil' _) -> return' init
        Foldl' t1 t2 g init (Cons' _ x xs) -> return' $ Foldl' t1 t2 g (App2 g init x) xs
        -- reduce `Len`
        Len' _ (Nil' _) -> return' Lit0
        Len' t (Cons' _ _ xs) -> return' $ Plus' Lit1 (Len' t xs)
        Len' _ (Range1' n) -> return' n
        -- reduce `At`
        At' t (Nil' _) i -> return' $ Bottom' t $ "cannot subscript empty list: index = " ++ formatExpr i
        At' t (Cons' _ x xs) i -> return' $ If' t (Equal' IntTy i Lit0) x (At' t xs (Minus' i Lit1))
        At' _ (Range1' _) i -> return' i
        -- reduce `Elem`
        Elem' _ _ (Nil' _) -> return' LitFalse
        Elem' t y (Cons' _ x xs) -> return' $ And' (Equal' t x y) (Elem' t y xs)
        Elem' _ x (Range1' n) -> return' $ And' (LessEqual' IntTy Lit0 x) (LessThan' IntTy x n)
        -- others
        Len' t (Build' _ _ base n) -> return' $ Plus' (Len' t base) n
        _ -> return Nothing
```

For example, a line `Len' _ (Nil' _) -> return' Lit0` represents a rewrite rule to replace a sub-expression `length []` with an expression `0`.
A line `Len' t (Cons' _ _ xs) -> return' $ Plus' Lit1 (Len' t xs)` represents a rewrite rule to replace a sub-expression `length (cons x xs)` with an expression `1 + length xs`.

Also, this rewrite rule `reduceFoldBuild` is rewritten [at `v5.2.0.0`](https://github.com/kmyk/Jikka/blob/4c0d00ae0cf8e0a0ab17b82bd0a3e31ceca11ace/src/Jikka/Core/Convert/ShortCutFusion.hs#L96-L114) with [Template Haskell](https://wiki.haskell.org/Template_Haskell), which is a macro feature of Haskell (GHC).
The content remains the same and the code is:

```haskell
reduceFoldMap :: MonadAlpha m => RewriteRule m
reduceFoldMap =
  mconcat
    [ -- reduce `Reversed`
      [r| "len/reversed" forall xs. len (reversed xs) = len xs |],
      [r| "elem/reversed" forall x xs. elem x (reversed xs) = elem x xs |],
      [r| "at/reversed" forall xs i. (reversed xs)[i] = xs[len(xs) - i - 1] |],
      -- reduce `Sorted`
      [r| "len/sorted" forall xs. len (sorted xs) = len xs |],
      [r| "elem/sorted" forall x xs. elem x (sorted xs) = elem x xs |],
      -- reduce `Map`
      [r| "len/map" forall f xs. len (map f xs) = len xs |],
      [r| "at/map" forall f xs i. (map f xs)[i] = f xs[i] |],
      [r| "foldl/map" forall g init f xs. foldl g init (map f xs) = foldl (fun y x -> g y (f x)) init xs|],
      -- others
      [r| "len/setat" forall xs i x. len xs[i <- x] = len xs |],
      [r| "len/scanl" forall f init xs. len (scanl f init xs) = len xs + 1 |],
      [r| "at/setat" forall xs i x j. xs[i <- x][j] = if i == j then x else xs[j] |]
    ]
```

- File: [src/Jikka/Core/Convert/ShortCutFusion.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/ShortCutFusion.hs) ([Jikka.Core.Convert.ShortCutFusion](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Convert-ShortCutFusion.html))

### Example of Implementation: Segment Tree

For example which treats data structures, let's see the implementation about segment trees.

The module [Jikka.Core.Convert.SegmentTree](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Convert-SegmentTree.html) has a function `reduceCumulativeSum`.
This function performs a conversion with segment trees, when cumulative sums are used in a [foldl](https://hackage.haskell.org/package/base/docs/Prelude.html#v:foldl) loop, but the target array of cumulative sums are updated in the loop and the cummulative sum cannot be moved out of the loop.

```python
def solve(n: int, a: List[int], q: int, l: List[int], r: List[int]) -> List[int]:
    for i in range(q):
        # a[l[i]] = sum(a[:r[i])
        b = [0]
        for j in range(n):
            b.append(b[j] + a[j])
        a[l[i]] = b[r[i]]
    return a
```

The function `reduceCumulativeSum` is implemented as follows [at `v5.1.0.0`](https://github.com/kmyk/Jikka/blob/795726a626ca3653555f6c5c176eb81de26b6d58/src/Jikka/Core/Convert/SegmentTree.hs#L123-L143):

```haskell
-- | `reduceCumulativeSum` converts combinations of cumulative sums and array assignments to segment trees.
reduceCumulativeSum :: (MonadAlpha m, MonadError Error m) => RewriteRule m
reduceCumulativeSum = RewriteRule $ \_ -> \case
  -- foldl (fun a i -> setat a index(i) e(a, i)) base incides
  Foldl' t1 t2 (Lam2 a _ i _ (SetAt' t (Var a') index e)) base indices | a' == a && a `isUnusedVar` index -> runMaybeT $ do
    let sums = listCumulativeSum (Var a) e -- (A)
    guard $ not (null sums)
    let semigrps = nub (sort (map fst sums))
    let ts = t2 : map SegmentTreeTy semigrps
    c <- lift $ genVarName a
    let proj i = Proj' ts i (Var c)
    let e' = replaceWithSegtrees a (zip semigrps (map proj [1 ..])) e -- (B)
    guard $ e' /= e
    e' <- lift $ substitute a (proj 0) e'
    b' <- lift $ genVarName a
    let updateSegtrees i semigrp = SegmentTreeSetPoint' semigrp (proj i) index (At' t (Var b') index) -- (C)
    let step = Lam2 c (TupleTy ts) i t1 (Let b' t2 (SetAt' t (proj 0) index e') (uncurryApp (Tuple' ts) (Var b' : zipWith updateSegtrees [1 ..] semigrps))) -- (D)
    b <- lift $ genVarName a
    let base' = Var b : map (\semigrp -> SegmentTreeInitList' semigrp (Var b)) semigrps -- (E)
    return $ Let b t2 base (Proj' ts 0 (Foldl' t1 (TupleTy ts) step (uncurryApp (Tuple' ts) base') indices)) -- (F)
  _ -> return Nothing
```

At first this function `reduceCumulativeSum` finds expressions in the form of `foldl (\a i -> setat a index(i) e(a, i)) base incides`, with the following entities:

- type `t`
- expression `base` (with type `[t]`)
- expression `indices` (with type `[Int]`)
- variable `a` (with type `[t]`)
- variable `i` (with type `Int`)
- builtin function `setat` (with type `[t] -> Int -> t -> [t]`)
- expression `index(i)` (may contain the variable `i` but doesn't contain the variable `a`. Its type is `Int`.)
- expression `e(a, i)` (may contain the variables `a` and `i`. Its type is `t`.)

At first, the function `reduceCumulativeSum` calls `listCumulativeSum` at (A) to list places where cumulative sums are used in `e(a, i)`.
Then it lists corresponding semigroups from them, and calls `replaceWithSegtrees` at (B) to replace cumulative sums in `e(a, i)` with expressions with segment trees.
It makes an expression to update the segment trees at (C), and makes a function body to give to `foldl` at (D).
Then it makes an initial state `base'` of segment trees at (E) line, and finally returns the result expression at (F).

To use segment trees here, the core language has [`data-structure` types](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Language-Expr.html#t:Type) and [builtin functions like `SegmentTreeInitList` `SegmentTreeGetRange` `SegmentTreeSetPoint`](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Language-Expr.html#t:Builtin).
For example, the builtin function `SegmentTreeSetPoint` has the type `segment−tree(S) → int → S → segment−tree(S)` for each `S: semigroup`.

Similarly, C++, to which the core language has been translated, has types and builtin functions for segment trees.

- File: [src/Jikka/Core/Convert/ShortCutFusion.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/ShortCutFusion.hs) ([Jikka.Core.Convert.SegmentTree](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Convert-SegmentTree.html))
- File: [src/Jikka/Core/Language/Expr.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Language/Expr.hs) ([Jikka.Core.Language.Expr](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Language-Expr.html))
- File: [src/Jikka/CPlusPlus/Language/Expr.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/CPlusPlus/Language/Expr.hs) ([Jikka.CPlusPlus.Language.Expr](https://kmyk.github.io/Jikka/haddock/Jikka-CPlusPlus-Language-Expr.html))

## 7. Convert the AST of our core languagee to a C++ AST

After optimizations, Jikka converts the AST of the core language to a C++ AST.

For example, consider the following code:

```haskell
solve :: Int -> Int
solve n =
    let a0 = 0
    in let b0 = 1
    in let (a3, b3) =
             foldl (\(a1, b1) _ ->
                 let c = a1 + b1
                 in let a2 = b1
                 in let b2 = c
                 in (a2, b2)
               ) (a0, b0) [0..n - 1]
    in a3
```

This is converted to the following C++ code:

```c++
int solve(int n) {
    int a0 = 0;
    int b0 = 1;
    pair<int, int> x = make_pair(a0, b0);
    for (int i = 0; i < n; ++ i) {
        auto [a1, b1] = x;
        int c = a1 + b1;
        int a2 = b1;
        int b2 = c;
        x = make_pair(a2, b2);
    }
    auto [a3, b3] = x;
    return a3;
}
```

- File: [src/Jikka/CPlusPlus/Convert/FromCore.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/CPlusPlus/Convert/FromCore.hs) ([Jikka.CPlusPlus.Convert.FromCore](https://kmyk.github.io/Jikka/haddock/Jikka-CPlusPlus-Convert-FromCore.html))

## 8. Postprocess the C++ AST

Jikka performs conversions to eliminate inefficiencies that occur in the conversion from a AST of the core language.
Mainly, it converts unnecessary copies to moves.
It also inserts necessary `#include` statements.

- File: [src/Jikka/CPlusPlus/Convert.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/CPlusPlus/Convert.hs) ([Jikka.CPlusPlus.Convert](https://kmyk.github.io/Jikka/haddock/Jikka-CPlusPlus-Convert.html))
- File: Conversion from copy to move [src/Jikka/CPlusPlus/Convert/MoveSemantics.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/CPlusPlus/Convert/MoveSemantics.hs) ([Jikka.CPlusPlus.Convert.MoveSemantics](https://kmyk.github.io/Jikka/haddock/Jikka-CPlusPlus-Convert-MoveSemantics.html))

### Example

A C++ AST just converted from the core language looks like the following C++ code:

```c++
int solve(int n) {
    int a0 = 0;
    int b0 = 1;
    pair<int, int> x = make_pair(a0, b0);
    for (int i = 0; i < n; ++ i) {
        auto [a1, b1] = x;
        int c = a1 + b1;
        int a2 = b1;
        int b2 = c;
        x = make_pair(a2, b2);
    }
    auto [a3, b3] = x;
    return a3;
}
```

This will be converted into an AST that corresponds to the following C++ code:

```c++
int solve(int n) {
    int a = 0;
    int b = 1;
    for (int i = 0; i < n; ++ i) {
        int c = a + b;
        a = b;
        b = c;
    }
    return a;
}
```

## 9. Convert the C++ AST to a C++ code

Finally Jikka converts C++ AST to C++ code.

We use the precedence value method for parentheses, as in [Text.Show](https://hackage.haskell.org/package/base/docs/Text-Show.html).

- File: [src/Jikka/CPlusPlus/Format.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/CPlusPlus/Format.hs) ([Jikka.CPlusPlus.Format](https://hackage.haskell.org/package/Jikka/docs/Jikka-CPlusPlus-Format.html))
