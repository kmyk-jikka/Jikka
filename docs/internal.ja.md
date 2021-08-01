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
[lex](https://ja.wikipedia.org/wiki/Lex) (その Haskell 版 [alex](https://www.haskell.org/alex/)) と [yacc](https://ja.wikipedia.org/wiki/Yacc) (同 [happy](https://www.haskell.org/happy/)) を用いて LALR(1) 構文解析器を生成して用いています。

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

いまのところ、最適化は rewrite rule などで変換可能な箇所を探して貪欲に変換をかけるという形で行われています。
つまり、DFS やビームサーチなどのような探索は行われていません。
探索を必要とするような複雑な最適化は今後の課題となっています。

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

- ファイル: [src/Jikka/Core/Convert/CumulativeSum.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/CumulativeSum.hs) ([Jikka.Core.Convert.CumulativeSum](https://kmyk.github.io/Jikka/Jikka-Core-Convert-CumulativeSum.html))
- ファイル: [src/Jikka/Core/Convert/BubbleLet.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/BubbleLet.hs) ([Jikka.Core.Convert.BubbleLet](https://kmyk.github.io/Jikka/Jikka-Core-Convert-BubbleLet.html))

### 具体的な実装コード例: Short Cut Fusion

[Short cut fusion](https://wiki.haskell.org/Short_cut_fusion) を行うための module [Jikka.Core.Convert.ShortCutFusion](https://kmyk.github.io/Jikka/Jikka-Core-Convert-ShortCutFusion.html) の実装を見てみましょう。
たとえばその中の `reduceFoldBuild` という rewrite rule は [`v5.1.0.0` の時点](https://github.com/kmyk/Jikka/blob/795726a626ca3653555f6c5c176eb81de26b6d58/src/Jikka/Core/Convert/ShortCutFusion.hs#L162-L183)では次のようになっています。

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

たとえば `Len' _ (Nil' _) -> return' Lit0` という行は `length []` という部分式を `0` という式で置き換えるという rewrite rule を、`Len' t (Cons' _ _ xs) -> return' $ Plus' Lit1 (Len' t xs)` という行は `length (cons x xs)` という部分式を `1 + length xs` という式で置き換えるという rewrite rule を表現しています。

- ファイル: [src/Jikka/Core/Convert/ShortCutFusion.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/ShortCutFusion.hs) ([Jikka.Core.Convert.ShortCutFusion](https://kmyk.github.io/Jikka/Jikka-Core-Convert-ShortCutFusion.html))

### 具体的な実装コード例: セグメント木

データ構造を扱う例として、セグメント木についての実装を見てみましょう。

Module [Jikka.Core.Convert.SegmentTree](https://kmyk.github.io/Jikka/Jikka-Core-Convert-SegmentTree.html) は関数 `reduceCumulativeSum` を持ちます。
これは [foldl](https://hackage.haskell.org/package/base/docs/Prelude.html#v:foldl) の中で累積和が使われているが、しかし累積和を取られている配列が動的に更新されるために単純に累積和を `foldl` の外には出せない場合 (たとえば次のような Python コードに対応するもの) に対し、セグメント木を用いた変形を施します。

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

関数 `reduceCumulativeSum ` は [`v5.1.0.0` の時点](https://github.com/kmyk/Jikka/blob/795726a626ca3653555f6c5c176eb81de26b6d58/src/Jikka/Core/Convert/SegmentTree.hs#L123-L143) で次のような実装になっています。

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

この関数 `reduceCumulativeSum ` は `foldl (\a i -> setat a index(i) e(a, i)) base incides` という形の式をまず探します。
ただしここに登場する型や式などは以下のようになります。

- 型 `t`
- 式 `base` (型は `[t]`)
- 式 `indices` (型は `[Int]`)
- 変数 `a` (型は `[t]`)
- 変数 `i` (型は `Int`)
- 組み込み関数 `setat` (型は `[t] -> Int -> t -> [t]`)
- 式 `index(i)` (変数 `i` のみを使って書かれ、変数 `a` は現れない、型は `Int`)
- 式 `e(a, i)` (変数 `a` および変数 `i` を使って書かれ、型は `t`)

関数 `reduceCumulativeSum ` は、まず冒頭の (A) の行で `listCumulativeSum` を呼んで式 `e(a, i)` 中で累積和が用いられている箇所を列挙します。
ここから対応する半群を抜き出し、そして (B) の行で `replaceWithSegtrees` を呼んで式 `e(a, i)` 中の累積和をセグメント木を利用する式で置き換えます。
また (C) の行でセグメント木を更新する式を作り、(D) の行で `foldl` に渡す関数の本体を作ります。
さらに (E) の行でセグメント木の初期状態を作るような式 `base'` を用意し、(F) の行で結果の式を作って返却します。

ここでセグメント木を用いるために core 言語には [`data-structure` 型](https://kmyk.github.io/Jikka/Jikka-Core-Language-Expr.html#t:Type) があり、また[組み込み関数 `SegmentTreeInitList` `SegmentTreeGetRange` `SegmentTreeSetPoint`](https://kmyk.github.io/Jikka/Jikka-Core-Language-Expr.html#t:Builtin) も用意されています。
たとえば組み込み関数 `SegmentTreeSetPoint` は `S: semigroup` に対し `segment−tree(S) → int → S → segment−tree(S)` という型を持ちます。

同様に、core 言語が変換されていく先である C++ においても、セグメント木に関連する型や組み込み関数が定義されています。

- ファイル: [src/Jikka/Core/Convert/ShortCutFusion.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/ShortCutFusion.hs) ([Jikka.Core.Convert.SegmentTree](https://kmyk.github.io/Jikka/Jikka-Core-Convert-SegmentTree.html))
- ファイル: [src/Jikka/Core/Language/Expr.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Language/Expr.hs) ([Jikka.Core.Language.Expr](https://kmyk.github.io/Jikka/Jikka-Core-Language-Expr.html))
- ファイル: [src/Jikka/CPlusPlus/Language/Expr.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/CPlusPlus/Language/Expr.hs) ([Jikka.CPlusPlus.Language.Expr](https://kmyk.github.io/Jikka/Jikka-CPlusPlus-Language-Expr.html))

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
