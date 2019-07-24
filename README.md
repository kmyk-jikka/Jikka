# Jikka

[![AppVeyor](https://ci.appveyor.com/api/projects/status/by1shf3c22xvwt19/branch/master?svg=true)](https://ci.appveyor.com/project/kmyk/jikka)

Jikka は 半自動で競技プログラミングの問題を解くすごいプログラムです。

競技プログラミングにおいて「ただ式変形をするだけで解ける」「ただデータ構造のライブラリを貼るだけで解ける」問題はわりとあります。
そのような問題を ML 系言語に似た形式で入力すると、計算量を落としつつ C++ の実装に変換して出力します。

現状

## 実行方法

.NET Core をインストール (<https://dotnet.microsoft.com/download>) してから

``` console
$ dotnet run < INPUT > OUTPUT
```

## 例

### 例題

問題:
自然数 k と長さ n の数列 a = (a₀, a₁, …, aₙ) が与えられます。
∑ᵢ˱ₖ maxⱼ˱ₙ (i + 2aⱼ) を計算してください。

入力 O(kn):

``` sml
let K = 100000 in
let given N : Nat in
let given A : N -> Nat in

sum K (fun i -> max N (fun j -> i + 2 * A j))
```

出力 O(k + n):

``` c++
int64_t solve(int64_t N, const vector<int64_t> & A) {
    int64_t K = 100000;
    int64_t a2 = 0;
    for (int64_t i2 = 0; i2 < K; ++ i2) {
        a2 += i2;
    }
    int64_t a1 = INT64_MIN;
    for (int64_t i1 = 0; i1 < N; ++ i1) {
        a1 = max(a1, 2 * A[i1]);
    }
    return a2 + K * a1;
}
```

### AtCoder Beginner Contest 134: C - Exception Handling

問題: <https://atcoder.jp/contests/abc134/tasks/abc134_c>

入力 O(N^2):

``` sml
let given N : [2, 200001) in
let given A : N -> 200001 in

let f (i : N) = max N (fun j -> if j = i then 0 else A j) in
f
```

出力 O(N): <https://atcoder.jp/contests/abc134/submissions/6526623>

## 機能

### 文法

現在 (v3) は ML 系を基本とした文法です。
BNF で書くと以下のような雰囲気になります。

``` ebnf
<program> ::= { <decl> } <expr>

<decl> ::= "let" <name> { <param> } [ ":" <type> ] "=" <expr> "in"
         | "let" "rec" <name> [ ":" <type> ] { "|" { <pattern> } "=>" <expr> } "in"
         | "let" "given" <name> ":" <type> "in"

<expr> ::= <name>
         | <expr> <expr>
         | "fun" <name> "->" <expr>
         | <expr> ("+" | "-" | "*" | ...) <expr>
         | "if" <expr> "then" <expr> "else" <expr>
         | <number>
         | "true"
         | "false"

<type> ::= "Nat"
         | "Zahl"
         | <expr>
         | "[" <expr> "," <expr> ")"
         | <type> "->" <type>
```

### 意味論

そのままです。

### 型

型には以下が使えます。

-   `Nat`: 自然数の全体の集合 ℕ です。
-   `Zahl`: 整数の全体の集合 ℤ です。
-   `n`: 自然数 n = { 0, 1, 2, …, n - 1 } ⊆ ℕ です。剰余類環 ℤ/nℤ とは異なります。
-   `[l, r)`: 整数上の半開区間 [l, r) = { l, l + 1, l + 2, …, r - 1 } ⊆ ℤ です。
-   `Bool`: 真偽値の集合 { false, true } です。便宜上 2 = { 0, 1 } とは異なります。
-   `S -> T`: S から T への関数の集合 { f | f : S → T } です。

注意として:

-   公理的集合論において行われるような種々の同一視を雑に行なっています。プログラムと関数とλ式と集合と論理式と型とはすべてだいたい同じものです。
-   `Nat` `n` `[l, r)` などの型は `Zahl` に注釈が付いたものとして実装されています。 (つまり篩型ですが、現状ではこの注釈はほとんどの場面で無視されます)
-   数列は関数 `A : n -> Zahl` として表現されます。長さ n の整数列 A = (a₀, a₁, …, aₙ₋₁) は f(i) = aᵢ という関数と同一視できます。
-   集合は関数 `A : U -> Bool` として表現されます。全体集合を U とする部分集合 A ⊆ U はその特性関数 χ(a) = 1 ⇔ a ∈ A と同一視できます。

### 組み込み関数

-   `+` `-` `*` `/` `%` `**`: 算術演算子です。
-   `=` `<>` `<` `<=` `>` `>=`: 比較演算子です。 ML の流儀に従い非等値判定 ≠ は `<>` で書かれます。
-   `!` `&&` `||`: 論理結合子です。
-   `count n p`: 集合 p ⊆ n の要素数 # p = # { i < n | p(i) } を計算します。
-   `sum n f`: 数列 f : n →  ℤ の総和 ∑ᵢ˱ₙ f(i) を計算します。
-   `max n f`: 数列 f : n →  ℤ の最大値 max { f(i) | i < n } を計算します。
-   `min n f`: 数列 f : n →  ℤ の最小値 min { f(i) | i < n } を計算します。
-   その他

未実装のもの

-   TODO: `argmax n f`
-   TODO: `argmin n f`

### 最適化

以下が実装されています。

-   定数畳み込み
-   大型演算子に対する種々の分配をする

目標

-   TODO: 数列の漸化式を自動で解く
-   TODO: 式の線形性や単調性を見抜く
-   TODO: データ構造を自動で選択して貼る

### 出力

C++ での実装が出力されます。
`let given` で定義された変数を引数としてプログラム全体の式の結果を値として返すような関数を出力します。

TODO: 計算量解析の結果も出力
