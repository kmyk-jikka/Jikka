# 数式を入力すると C++ での実装を出力してくれるすごいやつ

競技プログラミングにおいて「セグメント木を貼ると O(N^3) が O(N^2 log N) になって解けます」みたいなやつは親の顔よりたくさん見ました。そろそろ自動化しようと思います。


## 実行方法

.NET Core をインストール (<https://dotnet.microsoft.com/download>) してから

``` console
$ dotnet run
```


## 方向付けのための問いと答え

### Q. これは何ですか？

A. 競技プログラミングの問題の形式的な表現を受けとり、それに対する解法を出力するプログラムです。

### Q. これはどんな問題でも解けますか？

A. いいえ。形式化された問題のみを扱います。問題の形式化は競技プログラマ (あるいは他の自然言語処理プログラム) の役割です。

### Q. これは何のためのものですか？

A. 競技プログラマが問題を解くことを補助するためのものです。問題の考察の過程で出現した部分問題を高速に処理することで、競技プログラマが問題のより本質的な部分に注力することを助けます。

### Q. これを使うことで競技プログラミングにおけるレートは上がりますか？

A. はい。それを目指しています。

### Q. これはプログラミング言語の処理系ですか？

A. いいえ。これは、RDBMS が (たとえ SQL を扱っていたとしても) プログラミング言語の処理系とは認識されないのと同じです。

### Q. ではどのような種類のプログラムだと認識すべきでしょうか？

A. Vim や Emacs のようなエディタと同列に認識されるべきです。最終的には定理証明支援系のようになるでしょう。

### Q. 競技プログラミングはツールの利用の巧拙を競うものになるのでしょうか？

A. いいえ。機械的に解けてしまうような問題が出題されなくなるだけです。

### Q. レートで言うとどれくらいの能力がありますか？

A. 問題を解く過程全体の一部分しか処理しないためレートは定義できません。しかし、データ構造への理解が重要になる部分において AtCoder 換算で 1800 程度と言える水準を目指します。

### Q. 最終的には何を目指していますか？

A. (競技プログラミングの) 問題を解くという行為がどのようなものであるかの理解を深め、それをより効率的に行うことを目指しています。


## 例

### 例 (実装済み) (v2)

```
# Jikka v2
# https://atcoder.jp/contests/code-festival-2015-final-open/tasks/codefestival_2015_final_d

K = 100000
given N : Nat
given S : N -> Nat
given T : N -> (K + 1)
assume forall i. i < N implies S i < T i

f : N -> (K + 1) -> Nat
f i t = count (\ j. j < N and j /= i and S j <= t and t < T j)

compute min \ i. max \ t. f i t
```

``` console
$ dotnet run

{environment =
  [Define (Ident "K",[],IntExp 100000);
   Given (Ident "N",ExprTy (IdentExp (Ident "Nat")));
   Given ...;
   Given ...;
   Assume ...;
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
                (AppExp (IdentExp (Ident "f"),IdentExp (Ident "i")),
                 IdentExp (Ident "t"))))));}
```

### 例 (実装済み) (v1)

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

### 例 (目標) (v1)

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
