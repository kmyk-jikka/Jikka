# Jikka

Jikka は 半自動で競技プログラミングの問題を解くエキスパートシステムです。

競技プログラミングにおいて「ただ式変形をするだけで解ける」「ただデータ構造のライブラリを貼るだけで解ける」問題はわりとあります。
そのような問題を ML 系言語に似た形式で入力すると、計算量を落としつつ C++ の実装に変換して出力します。

## 実行方法

.NET Core をインストール (<https://dotnet.microsoft.com/download>) してから

``` console
$ dotnet run
```

## 例

### v3 (実装済み)

例題:
自然数 k と長さ n の数列 a = (a₀, a₁, …, aₙ) が与えられます。
∑ᵢ˱ₖ maxᵢ˱ₙ (i + 2aⱼ) を計算してください。

入力 O(kn):

```
# vim: set filetype=sml:
# Jikka v3

let K = 100000 in
let given N : Nat in
let given A : Nat -> Nat in

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
