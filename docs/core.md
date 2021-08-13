# About our core language

(このドキュメントの日本語バージョン: [docs/core.ja.md](https://github.com/kmyk/Jikka/blob/master/docs/core.ja.md))

Jikka converts input programs to our own intermediate language, core.
This core language is defined referring to [Core](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/core-syn-type), which is the intermediate language of a [Haskell](https://www.haskell.org/) compiler [GHC](https://www.haskell.org/ghc/).

## Syntax

Expressions of our core language are defined at [data `Expr`](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Language-Expr.html#t:Expr), and types are defined at [data `Type`](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Language-Expr.html#t:Type).
The list of builtin functions exist at [data `Builtin`](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Language-Expr.html#t:Builtin).

The toplevel environment is separated and defined at [data `ToplevelExpr`](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Language-Expr.html#t:ToplevelExpr).
`let rec` is available only at the toplevel.

## Semantics

Our core language is a statically-typed purely-functional programming language.
It works as almost the same to Haskell.

## String representation

The string representation is similar to OCaml.

- Types are like `unit` `int list` `int * int -> int`.
- Functions are `fun x y -> e`.
- Functions applications are `f x y`.
- `if e1 then e2 else e3` is an if-expression.
- `(e1, e2, e3)` is the tuple of `e1`, `e2` and `e3`.
- `e.i` is the `i`-th element of a tuple `e`.
- `xs[i]` is the `i`-th element of a list `xs`.
- `xs[i <- x]` is the list which is obtained by assining `x` to the `i`-th element of a list `xs`.
  - This notation comes from notations of substitutions which are used in discussion about λ calculus.
- List literals don't exist.
