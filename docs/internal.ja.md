# docs/internal.ja.md

Jikka の内部実装について解説します。

## 0. 全体の構成

Jikka の内部のおおまかな構成は以下を順に実行するものになっています。

1.  Python コードを読み込む
2.  Python コードの構文解析をして Python の構文木を得る
3.  Python の構文木を制限された Python の構文木に変換する
4.  制限された Python の構文木の前処理をする
5.  制限された Python の構文木を core 言語の構文木に変換する
6.  core 言語の構文木を最適化する
7.  core 言語の構文木を C++ の構文木に変換する
8.  C++ の構文木の後処理をする
9.  C++ の構文木を C++ コードに変換する
10. C++ コードを書き出す

「(標準の) Python」「制限された Python」「core 言語」「C++」をこの順に変換しています。
ただし、制限された Python は [docs/language.js.md](https://github.com/kmyk/Jikka/blob/master/docs/language.ja.md) で解説されているものです。
また core 言語はほとんど Haskell と言ってよいもので、これは Haskell のコンパイラである GHC の中間言語 [GHC Core](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/core-syn-type) に類似した中間言語になっています。

- modules の一覧 [Jikka](https://kmyk.github.io/Jikka/)
- ファイル: [src/Jikka/Main/Subcommand/Convert.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Main/Subcommand/Convert.hs) ([Jikka.Main.Subcommand.Convert](https://kmyk.github.io/Jikka/Jikka-Main-Subcommand-Convert.html))

## 2. Python コードの構文解析をして Python の構文木を得る

[Python の文法仕様](https://docs.python.org/ja/3/reference/grammar.html) に従い Python の構文解析をします。
[lex](https://ja.wikipedia.org/wiki/Lex) (その Hakell 版 [alex](https://www.haskell.org/alex/)) と [yacc](https://ja.wikipedia.org/wiki/Yacc) (同 [happy](https://www.haskell.org/happy/)) を用いて LALR(1) 構文解析器を生成して用いています。

- ファイル: lex [src/Jikka/Python/Parse/Happy.y](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Python/Parse/Happy.y) ([Jikka.Python.Parse.Happy](https://kmyk.github.io/Jikka/Jikka-Python-Parse-Alex.html))
- ファイル: yacc [src/Jikka/Python/Parse/Alex.x](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Python/Parse/Alex.x) ([Jikka.Python.Parse.Alex](https://kmyk.github.io/Jikka/Jikka-Python-Parse-Happy.html))
- 参考文献: [最新コンパイラ構成技法](https://www.amazon.co.jp/dp/4798114685) (通称: タイガーブック)

### 例

たとえば以下のような Python コードを考えてみましょう。

```python
def f(a, b) -> int:
    return a + b
```

ここからは以下のような構文木が得られます。
これは `$ python3 -c 'import ast; print(ast.dump(ast.parse("def f(a, b) -> int: return a + b")))'` とコマンドを実行することでも確認できます。

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

## 3. Python の構文木を制限された Python の構文木に変換する

Python を構文解析した段階では [`ast` module](https://docs.python.org/ja/3/library/ast.html) と同一の完全な構文木 ([data Expr](https://hackage.haskell.org/package/Jikka/docs/Jikka-Python-Language-Expr.html#t:Expr)) を得ています。
この構文木から不要な部分を削除し、我々の制限された Python のための扱いやすい構文木 ([data Expr](https://hackage.haskell.org/package/Jikka/docs/Jikka-RestrictedPython-Language-Expr.html#t:Expr)) を得ます。

- ファイル: [src/Jikka/Python/Convert/ToRestrictedPython.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Python/Convert/ToRestrictedPython.hs) ([Jikka.Python.Convert.ToRestrictedPython](https://kmyk.github.io/Jikka/Jikka-Python-Convert-ToRestrictedPython.html))

## 4. 制限された Python の構文木の前処理をする

制限された Python の構文木に対し、以下の処理をします。

1.  変数名の検査とリネーム
1.  [型推論](https://ja.wikipedia.org/wiki/%E5%9E%8B%E6%8E%A8%E8%AB%96)
1.  その他の細かい前処理と検査

型推論には Hindley/Milner 型推論アルゴリズムを用いています。
このアルゴリズムは、型変数についての等式を収集し、得られた連立方程式を解くことで型を復元します。

- ファイル: [src/Jikka/RestrictedPython/Convert.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/RestrictedPython/Convert.hs) [Jikka.RestrictedPython.Convert](https://kmyk.github.io/Jikka/Jikka-RestrictedPython-Convert.html)
- ファイル: 変数名の検査とリネーム [src/Jikka/RestrictedPython/Convert/Alpha.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/RestrictedPython/Convert/Alpha.hs) [Jikka.RestrictedPython.Convert.Alpha](https://kmyk.github.io/Jikka/Jikka-RestrictedPython-Convert-Alpha.html)
- ファイル: 型推論 [src/Jikka/RestrictedPython/Convert/TypeInfer.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/RestrictedPython/Convert/TypeInfer.hs) [Jikka.RestrictedPython.Convert.TypeInfer](https://kmyk.github.io/Jikka/Jikka-RestrictedPython-Convert-TypeInfer.html)
- 参考文献: [型システム入門 プログラミング言語と型の理論](https://www.amazon.co.jp/dp/B07CBB69SS) (通称: TaPL)

## 5. 制限された Python の構文木を core 言語の構文木に変換する

制限された Python の構文木を core 言語の構文木に変換します。

Python においてはたとえば代入文や `for` ループがありますが、core 言語 (Haskell) にはそれらはありません。
そのため、代入文はすべて `let` 文に、`for` ループは [foldl](https://hackage.haskell.org/package/base/docs/Prelude.html#v:foldl) に変換されます。

たとえば次のような Python コードから得られる構文木を考えましょう。

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

これは次のような Haskell コードに対応するような core 言語の構文木に変換されます。

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

- ファイル: [src/Jikka/RestrictedPython/Convert/ToCore.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/RestrictedPython/Convert/ToCore.hs) ([Jikka.RestrictedPython.Convert.ToCore](https://kmyk.github.io/Jikka/Jikka-RestrictedPython-Convert-ToCore.html))

## 6. core 言語の構文木を最適化する

Jikka の最適化の本体部分です。
思い付く限りのあらゆる最適化を手当たり次第に試します。
その多くは [rewrite rule](https://wiki.haskell.org/GHC/Using_rules) という形で書かれています。

- ファイル: [src/Jikka/Core/Convert.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert.hs) [Jikka.Core.Convert](https://kmyk.github.io/Jikka/Jikka-Core-Convert.html)
- ディレクトリ: [src/Jikka/Core/Convert/](https://github.com/kmyk/Jikka/tree/master/src/Jikka/Core/Convert)

### 例: 累積和

たとえば、次のような O(N²) の Python コードを考えてみましょう。

```python
def solve(n: int, a: List[int]) -> int:
    b = 0
    for i in range(n):
        b += sum(a[:i])
    return b
```

これはこの時点で次のような Haskell コードのような形に変換されています。

```haskell
solve :: Int -> [Int] -> Int
solve n a =
    foldl (\b i ->
        b + sum (map (\j -> a !! j) [0..i - 1])
      ) 0 [0..n - 1]
```

ここで累積和についての rewrite rule 「`sum (map (\i -> xs !! i) [0..k - 1])` という形の部分式があれば `let ys = scanl (+) 0 xs in ys !! k` という式で置き換えよ」が発動し、上のコードは次のような [scanl](https://hackage.haskell.org/package/base/docs/Prelude.html#v:scanl) を用いたコードに変換されます。

```haskell
solve :: Int -> [Int] -> Int
solve n a =
    foldl (\b i ->
        let c = scanl (+) 0 a
        in b + c !! i
      ) 0 [0..n - 1]
```

するとさらに [foldl](https://hackage.haskell.org/package/base/docs/Prelude.html#v:foldl) と `let` 式についての rewrite rule 「変数 `y` および変数 `x` が式 `c` の中で使われておらず、また変数 `a` が式 `y0` および式 `xs` の中で使われていないならば、部分式 `foldl (\y x -> let a = c in e) y0 xs` を式 `let a = c in foldl (\y x -> e) y0 xs` で置き換えよ」が発動し、次のようなコードに変換されます。

```haskell
solve :: Int -> [Int] -> Int
solve n a =
    let c = scanl (+) 0 a
    in foldl (\b i ->
           b + c !! i
         ) 0 [0..n - 1]
```

この結果の Haskell コードは、後に以下のような C++ コードへと変換されます。
これは O(N) です。

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

## 7. core 言語の構文木を C++ の構文木に変換する

core 言語の構文木を C++ の構文木に変換します。

たとえば次のようなコードを考えてみましょう。

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

これは次のような C++ コードに変換されます。

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

- ファイル: [src/Jikka/CPlusPlus/Convert/FromCore.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/CPlusPlus/Convert/FromCore.hs) [Jikka.CPlusPlus.Convert.FromCore](https://kmyk.github.io/Jikka/Jikka-CPlusPlus-Convert-FromCore.html)

## 8. C++ の構文木の後処理をする

core 言語の構文木から変換してきたときに発生して非効率的な部分を解消するような変換を行います。
主には不必要な copy を move に変換します。

- ファイル: [src/Jikka/CPlusPlus/Convert.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/CPlusPlus/Convert.hs) [Jikka.CPlusPlus.Convert](https://kmyk.github.io/Jikka/Jikka-CPlusPlus-Convert.html)
- ファイル: copy から move への変換 [src/Jikka/CPlusPlus/Convert/MoveSemantics.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/CPlusPlus/Convert/MoveSemantics.hs) [Jikka.CPlusPlus.Convert.MoveSemantics](https://kmyk.github.io/Jikka/Jikka-CPlusPlus-Convert-MoveSemantics.html)

### 例

core 言語から変換してきたばかりの C++ の構文木は以下のコードのようになっています。

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

これは以下の C++ コードに対応するような構文木に変換されます。

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

## 9. C++ の構文木を C++ コードに変換する

C++ の構文木を文字列に変換します。
括弧付けには [Text.Show](https://hackage.haskell.org/package/base/docs/Text-Show.html) のように precedence 値による方法を用いています。

- ファイル: [src/Jikka/CPlusPlus/Format.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/CPlusPlus/Format.hs) ([Jikka.CPlusPlus.Format](https://hackage.haskell.org/package/Jikka/docs/Jikka-CPlusPlus-Format.html))
