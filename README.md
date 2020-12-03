# Jikka

Jikka is an automated solver for problems of competitive programming.

In competitive programming, there are some problems which are solvable only with "repeating formula transformations", "pasting snippets of famous data structures", etc.
Jikka automatically solves such problems.
Jikka takes such problems as input in the form of a program of a very restricted subset of Python, optimizes the code to reduce the computational complexity, and outputs as an implementation in C++.
/
競技プログラミングにおいて「ただ式変形をするだけで解ける」「ただデータ構造のライブラリを貼るだけで解ける」問題は実は少なくありません。
Jikka はそのような問題を自動で解きます。
そのような問題をとても制限された Python のサブセット言語のコードの形で入力として受け取り、計算量を落とすような最適化を行い、C++ の実装に変換して出力します。

## Examples (`v3.1.0`)

The below are examples of old the version. The input was a ML code.

### 例題

Problem:
You are given a natural number K and a sequence A = (a₀, a₁, …, aₙ) of length N.
Compute the value of ∑ᵢ˱ₖ maxⱼ˱ₙ (i + 2 aⱼ).

Input, O(K N):

``` sml
let K = 100000 in
let given N : Nat in
let given A : N -> Nat in

sum K (fun i -> max N (fun j -> i + 2 * A j))
```

Output, O(K + N):

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

Problem: <https://atcoder.jp/contests/abc134/tasks/abc134_c>

Input, O(N^2):

``` sml
let given N : [2, 200001) in
let given A : N -> 200001 in

let f (i : N) = max N (fun j -> if j = i then 0 else A j) in
f
```

Output, O(N): <https://atcoder.jp/contests/abc134/submissions/6526623>
