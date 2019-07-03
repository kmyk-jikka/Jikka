# 数式を入力すると C++ での実装を出力してくれるすごいやつ

競技プログラミングにおいて「セグメント木を貼ると O(N^3) が O(N^2 log N) になって解けます」みたいなやつは親の顔よりたくさん見ました。そろそろ自動化しようと思います。

## 実行方法

.NET Core をインストール (<https://dotnet.microsoft.com/download>) してから

``` console
$ dotnet run
```

## 例 (実装済み) (v2)

```
# Jikka v2
# https://atcoder.jp/contests/code-festival-2015-final-open/tasks/codefestival_2015_final_d

K = 100001
given N : Nat
given S : N -> Nat
given T : N -> K
assume forall i. i < N implies S i < T i

f : K -> Nat
f t = count (\ i. i < N and S i <= t and t < T i)
g : N -> K -> Nat
g i t = f t - int (S i <= t and t < T i)

compute min \ i. max \ t. g i t
```

``` console
$ dotnet run
{environment =
  [Define (Ident "K",[],IntExp 100001);
   Given (Ident "N",ExprTy (IdentExp (Ident "Nat")));
   Given
     (Ident "S",
      FunTy (ExprTy (IdentExp (Ident "N")),ExprTy (IdentExp (Ident "Nat"))));
   Given
     (Ident "T",
      FunTy (ExprTy (IdentExp (Ident "N")),ExprTy (IdentExp (Ident "K"))));
   Assume ...;
   Declare ...;
   Define ...;
   Declare ...;
   Define ...];
 compute =
  AppExp
    (IdentExp (Ident "min"),
     LamExp
       (Ident "i",
        AppExp
          (IdentExp (Ident "max"),
           LamExp
             (Ident "t",
              AppExp
                (AppExp (IdentExp (Ident "g"),IdentExp (Ident "i")),
                 IdentExp (Ident "t"))))));}
```

## 例 (実装済み) (v1)

入力: `\sum _ {i < N} A_i`

出力:

``` c++
int64_t solve(const vector<int64_t> & A, int64_t N) {
    int64_t t0 = 0;
    for (int64_t i = 0; i < N; ++ i) {
        t0 += A[i];
    }
    return t0;
}
```

## 例 (目標)

入力:

``` tex
let f : (K + 1) \times N \to \mathbb{N}
    f(0, j) = A_j
    f(i + 1, j) = f(i, j) + \sum_{0 < k \le B_j} f(i, j - k)
in \sum_{j < N} f(N, j)
```

``` c++
struct segment_tree {
    ...
};

int64_t solve(int64_t N, const vector<int64_t> & A, const vector<int64_t> & B, int64_t K) {
    vector<vector<int64_t> > f(K + 1, vector<int64_t>(N));
    for (int64_t j = 0; j < N; ++ j) {
        f[0][j] = A[j];
    }
    for (int64_t i = 0; i < K; ++ i) {
        segment_tree segtree(N);
        for (int64_t j = 0; j < N; ++ j) {
            segtree.set(j, f[i][j]);
        }
        for (int64_t j = 0; j < N; ++ j) {
            f[i + 1][j] = f[i][j] + segtree.get(j - B[j], j);
        }
    }
    int64_t ans = 0;
    for (int64_t j = 0; j < N; ++ j) {
        ans += f[N][j];
    }
    return ans;
}
```
