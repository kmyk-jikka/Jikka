# Language Specification

(The English version of this document: [language.md](https://github.com/kmyk/Jikka/blob/master/docs/language.md))

## TL;DR

- Python と同じ構文を持つ。基本的には Python のサブセットである。
- OCaml や Haskell に似た意味論を持つ。静的型と型推論を持ち、すべての値は immutable である。
- 競技プログラミングのための組み込み関数や演算子を持つ。実装は漸近的計算量レベルの最適化を行うことが期待される。

## †破壊的変更†について

この言語仕様は開発の初期段階のものです。
第 2 のメジャーバージョンが 0 (`v5.0.y.z`) である限り、いつでも、いかなる変更も起こりえます (MAY)。
[Semantic Versioning の項目 (4.)](https://semver.org/#spec-item-4) も参考のこと。

## Syntax

### Lexical Analysis

Python とほとんど同じです。

### Grammer

基本的には Python と同じです。

多くの制限がありますが、通常の競技プログラミングにおいては問題にならないでしょう。
それらの制限は言語を単純化し最適化を容易にします。

### Expression

以下が利用可能です:

- literals
  - `0`, `1`, `2`, ...
  - `True`, `False`
- lists
  - `[a, b, c, ...]`
  - `[... for ... in ...]`
- generators
  - `(... for ... in ...)`
- subscriptions
  - `a[i]`
- function call
  - `f(a, b, c, ...)`
  - `f(... for ... in ...)`
- operators
  - See the "Standard Library" section.
- conditional operator
  - `... if ... eles ...`
- comprehensions with filtering
  - `... for ... in ... if ...`
- slicing
  - `a[l : r]`
- lambdas
  - `lambda x: ...`

以下は利用不可能です:

- list with stars
  - `[x, *xs]`
- walrus operator
  - `x := a`
- attribute reference
  - `foo.bar`
- complicated function calls
  - `f(foo=a, bar=b)`
  - `f(*args)`

### Simple statement

以下が利用可能です:

- assignment statement
  - `x = 0`
  - `a[y][x] = b`
  - `a, b = b, a`
- augmented assignment statement
  - `x += 1`
- annotated assignment statements
  - `xs: List[List[int]] = []`
- assert
  - `assert 0 <= n`
- pass
  - `pass`
- return
  - `return n + 1`
- import
  - `import ...`
  - `from ... import ...`

以下は利用不可能です:

- complicated assignment statement
  - `x, *xs = a`
  - `a[l : r] = b`
- expression statement
  - `print("Hello")`
- yield
- raise
- break
- continue

### Compound statement

以下が利用可能です:

- if
  - `if: ...`
  - `if cond: ... else: ...`
  - `if cond: ... elif cond: ... else: ...`
- for
  - `for x in xs: ...`
- def
  - `def f(x): ...`
  - `def f(x: t) -> t: ...`

以下は利用不可能です:

- while
- try
- with
- class

## Semantics (Statements)

### import

`import` 文は効果を持ちません。
それらは単に無視されます。

### def

`def` 文は関数を定義します。

- トップレベルでのみ使えます。
- 再帰を使うことができます。
- すでに定義されている関数を上書きすることはできません。
- 定義本体において、後に定義されることになる識別子を利用することはできません。これは相互再帰ができないことを意味します。

### return

`return` 文は `def` 文で定義される関数の返り値を宣言します。

- `def` 文中のすべてのコードパスは少なくともひとつの `return` 文を含まねばなりません。

### assignment

代入文 `x = a` は値 `a` を変数 `x` に束縛します。

- トップレベルおよび `def` 文の中で使えます。
- トップレベルではないなら、すでに定義されている変数を上書きし、また型を変更することができます。
- ML の `let x = a in ...` に相当します。
- 左辺値が添字を含む場合は augmented assignment として扱われます。

### augmented assignment

複合代入文 `x @= a` は値 `x @ a` を変数 `x` に束縛します。

- `def` 文の中でのみ使えます。

### if

`if` 文は実行を分岐させます。

- `if` 節あるいは `else` 節のどちらかでのみ定義されるような変数を `if` 文の後ろで参照することはできません。

### for

`for` 文は実行を繰り返します。

- `for` 文の中で新たに定義されるような変数を `for` 文の後ろで参照することはできません。
- `else` 節は利用できません。
- `for` 文の中では `return` 文は利用できません。

### assert

`assert` 文は未定義動作を導入します。

- 処理系は `assert` 文を最適化のヒントとして利用できます。

### `list.append`

`xs = xs + [x]` 文はリスト `xs` の末尾に要素 `x` を追加します。
この文は `xs.append(x)` あるいは `xs += [x]` と書くこともできます。

## Semantics (Exprs)

基本的には Python と同じです。

## Semantics (Types)

型は以下によって帰納的に定義されます。

- `int` は型です。この型は整数の全体からなる集合に対応します。
- `bool` は型です。この型は真偽値の全体からなる集合に対応します。
- 任意の型 `T` に対し、`List[T]` は型です。この型は `T` に所属する値の有限列の全体からなる集合に対応します。
- 非負整数 `n` および任意の型 `T1`, `T2`, ..., `Tn` に対し、`Tuple[T1, T2, ..., Tn]` は型です。この型は `T1`, `T2`, ..., `Tn` に対応する集合の直積集合に対応します。
- 非負整数 `n` および任意の型 `T1`, `T2`, ..., `Tn`, `R` に対し、`Callable[[T1, T2, ..., Tn], R]` は型です。この型は `T1`, `T2`, ..., `Tn` に対応する集合から `Tn` に対応する集合への `n` 変数関数の全体からなる集合に対応します。
- 以上で書かれたもののみが型です。

また、`None` という表記は `Tuple[]` という型の略記です。

すべての式や値はちょうどひとつの型に所属します (つまり Church-style です)。

## Entry points

### `solve` function

`solve` 関数はプログラムのエントリポイントです。

- すべてのプログラムには `solve` という名前の関数が定義されていなければなりません。
- `solve` 関数のそれぞれの引数の型は `int`, `List[int]`, `List[List[int]]`, `List[List[List[int]]]`, ... のいずれかでなければなりません。
- `solve` 関数の戻り値の型は `int`, `List[int]`, `List[List[int]]`, `List[List[List[int]]]`, ... のいずれか、あるいはそれらからなる `Tuple` でなければなりません。

### `main` function

`main` 関数は入出力フォーマットを指定するための特殊な関数です。

- `main` という名前の関数は定義されていなくてもかまいません。
- `main` 関数は引数を取ってはいけません。
- `main` 関数の戻り値の型は `None` でなければなりません。
- `main` 関数の中では以下に挙げる形の文のみが利用できます。
  - `x = int(input())`
  - `x, y, z = map(int, input().split())`
  - `xs = list(map(int, input().split())); assert len(xs) == n`
  - `xs = list(range(n)); for i in range(n): xs[i] = ...`
  - `x, y, z = solve(a, b, c)`
  - `print(x, y, z)`
  - `print(*xs)`
  - `for i in range(n): ...`

### Toplevel expression statements

プログラムのトップレベルには `main` 関数の呼び出し文を書くことができます。

これは以下のいずれかの形をしていなければなりません。

- `main()`
- `if __name__ == "__main__": main()`

## Standard Library

### builtin operators from Python

arithmetical operators (`Callable[[int], int]`, `Callable[[int, int], int]`):

- `-` negation
- `+` addition
- `-` subtraction
- `*` multiplication
- `//` division (floor, consistent to `%`)
  - `(x // y) * y + (x % y) == x` が常に成り立ちます。
- `%` modulo (always positive)
- `**` power

logical operators (`Callable[[bool], bool]`, `Callable[[bool, bool], bool]`):

- `not` not
- `and` and
  - 短絡評価は行なわれません。
- `or` or
  - 短絡評価は行なわれません。

bitwise operators (`Callable[[int], int]`, `Callable[[int, int], int]`):

- `~` bitwise-not
- `&` bitwise-and
- `|` bitwise-or
- `^` bitwise-xor
- `<<` left shift
- `>>` right shift

comparators (`Callable[[T, T], bool]`):

- `==` equal
- `!=` not-equal
- `<` less-than
- `>` greater-than
- `<=` less-or-equal
- `>=` greater-or-equal

the combinational operator (`Callable[[T, bool, T], T]`):

- `... if ... else ...`
  - 短絡評価は行なわれません。

### additional builtin operators

arithmetical operators (`Callable[[int, int], int]`):

- `/^` division (ceil)
  - `(x + y - 1) // y` と同じです。
- `%^` modulo (ceil, consistent to `/^`)
  - `(x /^ y) * y + (x %^ y) == x` が常に成り立ちます。
- `<?` min
  - [古い GCC](https://gcc.gnu.org/onlinedocs/gcc-3.3.2/gcc/Min-and-Max.html) に由来します。
  - 演算子 `<?` と `>?` は bit 演算子と比較演算子の中間の優先順位を持ち、左結合です。
    たとえば `a ^ 1 <? b == c` は `((a ^ 1) <? b) == c` です.
    また `a <? b <? c ?> d <? e` は `((((a <? b) <? c) ?> d) <? e)` です.
- `>?` max
  - same to min

logical operators (`Callable[[bool, bool], bool]`):

- `implies` implication
  - 短絡評価は行なわれません。
  - 演算子 `implies` は `or` 演算子と `if`-`else` の中間の優先順位を持ち、右結合です。
    たとえば `a implies b if c else d implies e` は `(a implies b) if (c) eles (d implies e)` です。
    また `a implies b implies c` は `a implies (b implies c)` です。

### builtin functions from Python

integer functions:

- `abs(x: int) -> int`
- `min(x: T, y: T) -> T`
- `max(x: T, y: T) -> T`
- `pow(x: int, y: int) -> int`
- `pow(x: int, y: int, mod: int) -> int`
- `math.gcd(x: int, y: int) -> int`
- `math.lcm(x: int, y: int) -> int`

list functions:

- `len(xs: List[T]) -> int`
- `sum(xs: List[int]) -> int`
- `min(xs: List[T]) -> T`
- `max(xs: List[T]) -> T`
- `all(xs: List[bool]) -> bool`
- `any(xs: List[bool]) -> bool`
- `sorted(xs: List[T]) -> List[T]`
- `list(xs: List[T]) -> List[T]`
- `reversed(xs: List[T]) -> List[T]`
- `range(stop: int) -> List[int]`
- `range(start: int, stop: int) -> List[int]`
- `range(start: int, stop: int, step: int) -> List[int]`

### additional builtin functions

basic integer functions:

- `floordiv(x: int, y: int) -> int`
  - `x // y` と同じです。
- `floormod(x: int, y: int) -> int`
  - `x % y` と同じです。
- `ceildiv(x: int, y: int) -> int`
  - `(x + y - 1) // y` と同じです。
- `ceilmod(x: int, y: int) -> int`

list functions:

- `product(xs: Iterator[int]) -> int`
- `jikka.argmin(xs: List[T]) -> int`
  - `xs` は空であってはいけません。
- `jikka.argmax(xs: List[T]) -> int`
  - `xs` は空であってはいけません。

combinatorics functions:

- `jikka.fact(x: int) -> int`
- `jikka.choose(n: int, r: int): int`
  - `n >= r` が成り立っていなければなりません。
- `jikka.permute(n: int, r: int) -> int`
  - `n >= r` が成り立っていなければなりません。
- `jikka.multichoose(n: int, r: int) -> int`
  - `n >= r` が成り立っていなければなりません。

modular functions:

- `modinv(x: int, mod: int) -> int`
  - `x` は `mod` の倍数であってはいけません。
  - `mod` は正でなければなりません。

(現在の実装においては、接頭辞 `jikka.` は自由に付け外しできます。)

TODO:

- matrix functions:
- matrix functions on modular arithmetic:
