# About our core language

(The English version of this document: [docs/core.md](https://github.com/kmyk/Jikka/blob/master/docs/core.md))

Jikka は入力されたプログラムを内部で独自の中間言語 core に変換して処理します。
この core 言語は、[Haskell](https://www.haskell.org/) のコンパイラである [GHC](https://www.haskell.org/ghc/) の中間言語 [Core](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/core-syn-type) を参考にして定義されています。

## Syntax

core 言語の式は [data `Expr`](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Language-Expr.html#t:Expr) で定義されており、型は [data `Type`](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Language-Expr.html#t:Type) で定義されています。
組み込み関数の一覧は [data `Builtin`](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Language-Expr.html#t:Builtin) にあります。

また、toplevel 環境は [data `ToplevelExpr`](https://kmyk.github.io/Jikka/haddock/Jikka-Core-Language-Expr.html#t:ToplevelExpr) で定義されて分離されています。
`let rec` は toplevel でのみ利用可能です。

## Semantics

core 言語は静的型付き純粋関数型プログラミング言語です。
Haskell とほとんど同様に動作します。

## String representation

文字列表現は OCaml 寄りの独自のものを採用しています。

- 型は `unit` `int list` `int * int -> int` などと書きます。
- 関数は `fun x y -> e` のように書きます。
- 関数適用 `f x y` のように書きます。
- `if e1 then e2 else e3` は if 式を表します。
- `(e1, e2, e3)` は `e1` `e2` `e3` からなるタプルを表します。
- `e.i` はタプル `e` の `i` 要素目を表します。
- `xs[i]` はリスト `xs` の `i` 要素目を表します。
- `xs[i <- x]` はリスト `xs` の `i` 要素目に `x` を代入して得られるようなリストを表します。
  - この記法は λ 計算の議論で用いられる置換の記法に由来しています。
- リストリテラルはありません。
