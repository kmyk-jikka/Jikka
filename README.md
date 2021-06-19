# Jikka

Jikka is an automated solver for problems of competitive programming.

In competitive programming, there are some problems which are solvable only with "repeating formula transformations", "pasting snippets of famous data structures", etc.
Jikka automatically solves such problems.
Jikka takes such problems as input in the form of a program of a very restricted subset of Python, optimizes the code to reduce the computational complexity, and outputs as an implementation in C++.
/
競技プログラミングにおいて「ただ式変形をするだけで解ける」「ただデータ構造のライブラリを貼るだけで解ける」問題は実は少なくありません。
Jikka はそのような問題を自動で解きます。
そのような問題をとても制限された Python のサブセット言語のコードの形で入力として受け取り、計算量を落とすような最適化を行い、C++ の実装に変換して出力します。


## Usage

``` console
$ stack run convert PYTHON_FILE
```

[Stack](https://www.haskellstack.org/) is required.


## Documents

for users:

-   [docs/language.md](https://github.com/kmyk/Jikka/blob/master/docs/language.md)
-   [CHANGELOG.md](https://github.com/kmyk/Jikka/blob/master/CHANGELOG.md)

for developpers:

-   [CONTRIBUTING.md](https://github.com/kmyk/Jikka/blob/master/CONTRIBUTING.md)
-   [docs/DESIGN.md](https://github.com/kmyk/Jikka/blob/master/docs/DESIGN.md) (Japanese)
-   [docs/how-it-works.pdf](https://github.com/kmyk/Jikka/blob/master/docs/how-it-works.pdf) (Japanese)


## Examples (`v3.1.0`)

The below are examples of old the version (at `v3.1.0`). The input was a ML code.

### Sum of Max

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


## Examples (`v5.0.1.0`)

``` console
$ cat examples/fact.py
def f(n: int) -> int:
    if n == 0:
        return 1
    else:
        return n * f(n - 1)

$ stack run convert examples/fact.py
int64_t f0_f(int64_t a1_n) {
    bool x2 = a1_n == 0;
    if (x2) {
        return 1;
    } else {
        int64_t x3 = - 1;
        int64_t x4 = x3;
        int64_t x5 = x4;
        int64_t x6 = a1_n + x5;
        int64_t x7 = x6;
        int64_t x8 = f0_f(x7);
        return a1_n * x8;
    }
}
int64_t solve(int64_t a9) {
    return f0_f(a9);
}
```


## License

Appache License 2.0
