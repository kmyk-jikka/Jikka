# Changelog for Jikka

## 2021-06-25: v5.0.5.0

Some optimizations are implemented.
Now it can convert a O(N) Python code for fibonacci to O(log N) C++ code.

Input, O(N):

``` python
def f(n: int) -> int:
    a = 0
    b = 1
    for _ in range(n):
        c = a + b
        a = b
        b = c
    return a

def solve(n: int) -> int:
    return f(n) % 1000000007
```

Output, O(log N):

``` c++
#include "jikka/all.hpp"
#include <algorithm>
#include <cstdint>
#include <functional>
#include <iostream>
#include <numeric>
#include <string>
#include <tuple>
#include <vector>
int64_t solve(int64_t n_317) {
  return jikka::modmatap<2, 2>(
      jikka::modmatpow<2>(jikka::make_array<std::array<int64_t, 2>>(
                              jikka::make_array<int64_t>(1, 1),
                              jikka::make_array<int64_t>(1, 0)),
                          n_317, 1000000007),
      jikka::make_array<int64_t>(1, 0), 1000000007)[1];
}
int main() {
  int64_t x318;
  std::cin >> x318;
  int64_t x319 = solve(x318);
  std::cout << x319;
  std::cout << '\n';
}
```

## 2021-06-23: v5.0.4.0

Now executable C++ code is generated.

## 2021-06-23: v5.0.3.0

Now the conversion from restricted Python to core works.

## 2021-06-19: v5.0.2.0

Most conversions in restricted Python are implemented.

## 2020-12-03: v5.0.1.0

`v5.0.1.0` is the first version of the third prototype.
This version is a more realistic one, which reads a very restricted subset of Python.

## 2020-04-30: v4.0.1.0

`v4.0.1.0` aims to the same thing to `v3.x`, but it's restarted with Haskell.
However, this version didn't reach the usable version.

## 2019-07-24: v3.1.0

`v3.1.0` is the first version which seems to be somewhat usable in practice.

Input O(N^2):

``` sml
let given N : [2, 200001) in
let given A : N -> 200001 in

let f (i : N) = max N (fun j -> if j = i then 0 else A j) in
f
```

Output O(N):

The generated function (+ main function written by hands) gets AC: <https://atcoder.jp/contests/abc134/submissions/6526623>

``` c++
vector<int64_t> solve(int64_t N, const vector<int64_t> & A) {
    vector<int64_t> t1(N + 1);
    t1[0] = INT64_MIN;
    for (int i1 = 0; i1 < N + 1 - 1; ++ i1) {
        t1[i1 + 1] = ((0 <= i1) ? max<int64_t>(t1[i1], A[i1]) : INT64_MIN);
    }
    auto & g1 = t1;
    vector<int64_t> t2(N + 1);
    t2[0] = INT64_MIN;
    for (int i2 = 0; i2 < N + 1 - 1; ++ i2) {
        t2[i2 + 1] = ((((N - i2) - 1) < N) ? max<int64_t>(t2[i2], A[((N - i2) - 1)]) : INT64_MIN);
    }
    auto & g2 = t2;
    vector<int64_t> t3(N);
    for (int i3 = 0; i3 < N; ++ i3) {
        t3[i3] = max<int64_t>(max<int64_t>(max<int64_t>(INT64_MIN, g1[i3]), g2[(((N - (i3 + 1)) - 1) + 1)]), 0);
    }
    auto & f = t3;
    return f;
}
```

## 2019-07-19: v3.0.0

`v3.0.0` writes C++ function.

Input O(k n):

``` sml
# vim: set filetype=sml:
# Jikka v3

let K = 100000 in
let given N : Nat in
let given A : Nat -> Nat in

sum K (fun i -> max N (fun j -> i + 2 * A j))
```

Output O(k + n):

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

## 2019-07-10: v2

>   競技プログラミングの問題の形式的な表現を受けとり、それに対する解法を出力するプログラムです。

`v2` is the second prototype.
This version reads a mathematical expression written in ML-like language, and only writes a internal AST.

Input:

``` ml
# Jikka v2
# https://atcoder.jp/contests/code-festival-2015-final-open/tasks/codefestival_2015_final_d

K = 100000
input N : Nat
input S : N -> Nat
K1 = K + 1
input T : N -> K1
assume forall i. i < N implies S i < T i

f : N -> K1 -> Nat
f i t = count N (\ j. j < N and j /= i and S j <= t and t < T j)

output min N \ i. max K1 \ t. f i t
```

``` console

$ dotnet run

{compiletime =
  [(Ident "K1",
    AppBExp
      (AppBExp
         (FreeVarBExp (Ident "+",FunBTy (IntBTy,FunBTy (IntBTy,IntBTy))),
          FreeVarBExp (Ident "K",IntBTy)),IntBExp 1), BaseBScm IntBTy);
   (Ident "K", IntBExp 100000, BaseBScm IntBTy)];
 input =
  [(Ident "T", FunBTy (VarBTy (Ident "N"),VarBTy (Ident "K1")));
   (Ident "S", FunBTy (VarBTy (Ident "N"),VarBTy (Ident "Nat")));
   (Ident "N", VarBTy (Ident "Nat"))];
 runtime =
  [...];
 assumptions =
  [...];
 output =
  (AppBExp
     (AppBExp
        (FreeVarBExp
           (Ident "min",FunBTy (IntBTy,FunBTy (FunBTy (IntBTy,IntBTy),IntBTy))),
         FreeVarBExp (Ident "N",IntBTy)),
      LamBExp
        (IntBTy,
         AppBExp
           (AppBExp
              (FreeVarBExp
                 (Ident "max",
                  FunBTy (IntBTy,FunBTy (FunBTy (IntBTy,IntBTy),IntBTy))),
               FreeVarBExp (Ident "K1",IntBTy)),
            LamBExp
              (IntBTy,
               AppBExp
                 (AppBExp
                    (FreeVarBExp
                       (Ident "f",FunBTy (IntBTy,FunBTy (IntBTy,IntBTy))),
                     VarBExp 1),VarBExp 0))))), IntBTy);}
```

## 2019-07-02: v1

>   数式を入力すると C++ での実装を出力してくれるすごいやつ

`v1` is the first prototype.
This version reads a mathematical expression written in TeX-like notation, and writes a C++ function.
It is implemented with F#.

Input:

```
\sum _ {i < N} A_i
```

Output:

``` c++
int64_t solve(const vector<int64_t> & A, int64_t N) {
    int64_t t0 = 0;
    for (int64_t i = 0; i < N; ++ i) {
        t0 += A[i];
    }
    return t0;
}
```
