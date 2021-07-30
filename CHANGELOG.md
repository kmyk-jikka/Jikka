# Changelog for Jikka

## 2021-07-28: v5.0.11.1

Uploaded to Hackage: <https://hackage.haskell.org/package/Jikka>

## 2021-07-21: v5.0.11.0

Convex Hull Trick and Segment Trees are implemented.

Input (`examples/dp_z.py`):

```python
# https://atcoder.jp/contests/dp/tasks/dp_z
from typing import *

INF = 10 ** 18

def solve(n: int, c: int, h: List[int]) -> int:
    assert 2 <= n <= 10 ** 5
    assert 1 <= c <= 10 ** 12
    assert len(h) == n
    assert all(1 <= h_i <= 10 ** 6 for h_i in h)

    dp = [INF for _ in range(n)]
    dp[0] = 0
    for i in range(1, n):
        for j in range(i):
            dp[i] = min(dp[i], dp[j] + (h[j] - h[i]) ** 2 + c)
    return dp[n - 1]

def main() -> None:
    n, c = map(int, input().split())
    h = list(map(int, input().split()))
    assert len(h) == n
    ans = solve(n, c, h)
    print(ans)

if __name__ == '__main__':
    main()
```

Output (<https://atcoder.jp/contests/dp/submissions/24563891>):

```c++
#include "jikka/base.hpp"
#include "jikka/convex_hull_trick.hpp"
#include <algorithm>
#include <array>
#include <cstdint>
#include <functional>
#include <iostream>
#include <numeric>
#include <string>
#include <tuple>
#include <vector>
int64_t solve(int64_t n_0, int64_t c_1, std::vector<int64_t> h_2) {
  std::vector<int64_t> x3;
  x3.push_back(0);
  jikka::convex_hull_trick x4_10;
  for (int32_t x5 = 0; x5 < n_0 - 1; ++x5) {
    x4_10.add_line(h_2[x5], h_2[x5] * h_2[x5] + x3[x5]);
    x3.push_back(c_1 + h_2[(x5 + 1)] * h_2[(x5 + 1)] +
                 x4_10.get_min(h_2[(x5 + 1)] * -2));
  }
  return x3[(n_0 - 1)];
}
int main() {
  int64_t n_12 = -1;
  int64_t c_13 = -1;
  std::cin >> n_12;
  std::vector<int64_t> h_14(n_12, -1);
  std::cin >> c_13;
  for (int32_t i15 = 0; i15 < n_12; ++i15) {
    std::cin >> h_14[i15];
  }
  auto ans_16 = solve(n_12, c_13, h_14);
  std::cout << ans_16 << ' ';
  std::cout << '\n' << ' ';
}
```

Input (`examples/dp_q.py`):

```python
# https://atcoder.jp/contests/dp/tasks/dp_q
from typing import *

def solve(n: int, h: List[int], a: List[int]) -> int:
    assert 1 <= n <= 2 * 10 ** 5
    assert len(h) == n
    assert all(1 <= h_i <= n for h_i in h)
    assert len(a) == n
    assert all(1 <= a_i <= 10 ** 9 for a_i in a)

    dp = [0 for _ in range(n)]
    for i in range(n):
        b = 0
        for j in range(h[i]):
            b = max(b, dp[j])
        dp[h[i] - 1] = b + a[i]
    return max(dp)

def main() -> None:
    n = int(input())
    h = list(map(int, input().split()))
    assert len(h) == n
    a = list(map(int, input().split()))
    assert len(a) == n
    ans = solve(n, h, a)
    print(ans)

if __name__ == '__main__':
    main()
```

Output (<https://atcoder.jp/contests/dp/submissions/24561829>):

```c++
#include "jikka/base.hpp"
#include "jikka/segment_tree.hpp"
#include <algorithm>
#include <array>
#include <atcoder/segtree>
#include <cstdint>
#include <functional>
#include <iostream>
#include <numeric>
#include <string>
#include <tuple>
#include <vector>
int64_t solve(int64_t n_0, std::vector<int64_t> h_1, std::vector<int64_t> a_2) {
  std::vector<int64_t> x4(n_0, 0);
  atcoder::segtree<int64_t, jikka::max_int64_t, jikka::const_int64_min> x5_13(
      x4);
  for (int32_t x6 = 0; x6 < n_0; ++x6) {
    x4[(h_1[x6] - 1)] = std::max<int64_t>(0, x5_13.prod(0, h_1[x6])) + a_2[x6];
    x5_13.set(h_1[x6] - 1, x4[(h_1[x6] - 1)]);
  }
  int64_t x11 = *std::max_element(x4.begin(), x4.end());
  return x11;
}
int main() {
  int64_t n_14 = -1;
  std::cin >> n_14;
  std::vector<int64_t> h_15(n_14, -1);
  std::vector<int64_t> a_16(n_14, -1);
  for (int32_t i17 = 0; i17 < n_14; ++i17) {
    std::cin >> h_15[i17];
  }
  for (int32_t i18 = 0; i18 < n_14; ++i18) {
    std::cin >> a_16[i18];
  }
  auto ans_19 = solve(n_14, h_15, a_16);
  std::cout << ans_19 << ' ';
  std::cout << '\n' << ' ';
}
```

## 2021-07-21: v5.0.10.0

- The generated C++ code is optimized.
- `list.append` is added in the restricted Python.

## 2021-07-14: v5.0.9.0

- The generated C++ code becomes more natural.
- The restricted Python allows to write `main` function and uses it to analyze input/output format.

Input:

```python
# https://atcoder.jp/contests/dp/tasks/dp_a
from typing import *

def solve(n: int, h: List[int]) -> int:
    assert 2 <= n <= 10 ** 5
    assert len(h) == n
    assert all(1 <= h_i <= 10 ** 4 for h_i in h)

    dp = [-1 for _ in range(n)]
    dp[0] = 0
    dp[1] = abs(h[1] - h[0])
    for i in range(2, n):
        dp[i] = min(dp[i - 1] + abs(h[i] - h[i - 1]), dp[i - 2] + abs(h[i] - h[i - 2]))
    return dp[n - 1]

def main() -> None:
    n = int(input())
    h = list(map(int, input().split()))
    assert len(h) == n
    ans = solve(n, h)
    print(ans)

if __name__ == '__main__':
    main()
```

Output (<https://atcoder.jp/contests/dp/submissions/24221651>):

```c++
#include "jikka/all.hpp"
#include <algorithm>
#include <cstdint>
#include <functional>
#include <iostream>
#include <numeric>
#include <string>
#include <tuple>
#include <vector>
int64_t solve(int64_t n_0, std::vector<int64_t> h_1) {
  int64_t x6;
  if (n_0 + -1 == 0) {
    x6 = 0;
  } else {
    std::vector<std::array<int64_t, 2>> x2 =
        std::vector<std::array<int64_t, 2>>(n_0 + -2 + 1);
    x2[0] = jikka::make_array<int64_t>(
        0, std::max<int64_t>(-h_1[0] + h_1[1], h_1[0] + -h_1[1]));
    for (int32_t i3 = 0; i3 < int32_t(n_0 + -2); ++i3) {
      x2[(i3 + 1)] = jikka::make_array<int64_t>(
          x2[i3][1],
          std::min<int64_t>(
              x2[i3][1] + std::max<int64_t>(-h_1[(i3 + 1)] + h_1[(i3 + 2)],
                                            h_1[(i3 + 1)] + -h_1[(i3 + 2)]),
              x2[i3][0] + std::max<int64_t>(-h_1[i3] + h_1[(i3 + 2)],
                                            h_1[i3] + -h_1[(i3 + 2)])));
    }
    x6 = x2[(n_0 + -2)][1];
  }
  return x6;
}
int main() {
  int64_t n_7 = -1;
  std::cin >> n_7;
  std::vector<int64_t> h_8 = std::vector<int64_t>(n_7, -1);
  for (int32_t i9 = 0; i9 < n_7; ++i9) {
    std::cin >> h_8[i9];
  }
  auto ans_10 = solve(n_7, h_8);
  std::cout << ans_10 << ' ';
  std::cout << '\n' << ' ';
}
```

## 2021-07-11: v5.0.8.0

Some optimizers are added.
Now it can use cumulative sums.

Input:

```python
# https://judge.yosupo.jp/problem/static_range_sum

from typing import *

def solve(n: int, q: int, a: List[int], l: List[int], r: List[int]) -> List[int]:
    ans = [-1 for _ in range(q)]
    for i in range(q):
        ans[i] = sum(a[l[i]:r[i]])
    return ans
```

Output (<https://judge.yosupo.jp/submission/52832>):

```c++
std::vector<int64_t> solve(int64_t n_1653, int64_t q_1654,
                           std::vector<int64_t> a_1655,
                           std::vector<int64_t> l_1656,
                           std::vector<int64_t> r_1657) {
  std::vector<int64_t> x1658 = jikka::scanl<int64_t, int64_t>(
      [=](int64_t b1659) -> std::function<int64_t(int64_t)> {
        return [=](int64_t b1660) -> int64_t { return b1659 + b1660; };
      },
      0, a_1655);
  return jikka::fmap<int64_t, int64_t>(
      [=](int64_t b1661) -> int64_t {
        return x1658[(r_1657[b1661] + (-l_1656[b1661] + l_1656[b1661]))] +
               -x1658[l_1656[b1661]];
      },
      jikka::range1(q_1654));
}
```

## 2021-07-09: v5.0.7.0

Many internal cleanups are done.

Now our core language is very close to GHC' Core.
It's curried and has a system for rewrite-rules.

## 2021-06-29: v5.0.6.0

Error reporting and error recovery are improved.

Input:

```python
def solve(n: int) -> bool:
    a = n + True  # err
    b = 2 * n
    return b  # err
```

Output:

```console
Type Error (line 2 column 13) (user's mistake?): Jikka.RestrictedPython.Convert.TypeInfer: failed to solve type equations: failed to unify type int and type bool: type int is not type bool
1 |def solve(n: int) -> bool:
2 |    a = n + True  # err
               ^^^^
3 |    b = 2 * n

Type Error (line 4 column 12) (user's mistake?): Jikka.RestrictedPython.Convert.TypeInfer: failed to solve type equations: failed to unify type bool and type int: type bool is not type int
3 |    b = 2 * n
4 |    return b  # err
              ^
```

contributions:

- @Koki-Yamaguchi fixed build on macOS ([#28](https://github.com/kmyk/Jikka/pull/28))

## 2021-06-25: v5.0.5.0

Some optimizations are implemented.
Now it can convert a O(N) Python code for fibonacci to O(log N) C++ code.

Input, O(N):

```python
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

```c++
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

```sml
let given N : [2, 200001) in
let given A : N -> 200001 in

let f (i : N) = max N (fun j -> if j = i then 0 else A j) in
f
```

Output O(N):

The generated function (+ main function written by hands) gets AC: <https://atcoder.jp/contests/abc134/submissions/6526623>

```c++
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

```sml
# vim: set filetype=sml:
# Jikka v3

let K = 100000 in
let given N : Nat in
let given A : Nat -> Nat in

sum K (fun i -> max N (fun j -> i + 2 * A j))
```

Output O(k + n):

```c++
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

> 競技プログラミングの問題の形式的な表現を受けとり、それに対する解法を出力するプログラムです。

`v2` is the second prototype.
This version reads a mathematical expression written in ML-like language, and only writes a internal AST.

Input:

```ml
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

```console

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

> 数式を入力すると C++ での実装を出力してくれるすごいやつ

`v1` is the first prototype.
This version reads a mathematical expression written in TeX-like notation, and writes a C++ function.
It is implemented with F#.

Input:

```
\sum _ {i < N} A_i
```

Output:

```c++
int64_t solve(const vector<int64_t> & A, int64_t N) {
    int64_t t0 = 0;
    for (int64_t i = 0; i < N; ++ i) {
        t0 += A[i];
    }
    return t0;
}
```
