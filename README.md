# Jikka

[![test](https://github.com/kmyk/Jikka/actions/workflows/test.yml/badge.svg)](https://github.com/kmyk/Jikka/actions/workflows/test.yml)

Jikka is an automated solver for problems of competitive programming.

In competitive programming, there are some problems which are solvable only with "repeating formula transformations", "pasting snippets of famous data structures", etc.
Jikka automatically solves such problems.
Jikka takes such problems as input in the form of a program of a very restricted subset of Python, optimizes the code to reduce the computational complexity, and outputs as an implementation in C++.
/
競技プログラミングにおいて「ただ式変形をするだけで解ける」「ただデータ構造のライブラリを貼るだけで解ける」問題は実は少なくありません。
Jikka はそのような問題を自動で解きます。
そのような問題をとても制限された Python のサブセット言語のコードの形で入力として受け取り、計算量を落とすような最適化を行い、C++ の実装に変換して出力します。

## Usage

```console
$ jikka convert PYTHON_FILE
```

## How to Install

### Using prebuilt binaries (recommended)

Go [Releases page](https://github.com/kmyk/Jikka/releases) and download `jikka-vA.B.C.D-Linux`, `jikka-vA.B.C.D-maxOS` or `jikka-vA.B.C.D-Windows.exe`.

### Using Stack

[Stack](https://www.haskellstack.org/) is required. If you are using Ubuntu, you can install Stack with `$ sudo apt install haskell-stack`.

```console
$ git clone https://github.com/kmyk/Jikka.git
$ cd Jikka
$ stack install
```

### Using Cabal

[Cabal](https://www.haskell.org/cabal/) is required. This is bundled [Haskell Platform](https://www.haskell.org/platform/). If you are using Ubuntu, you can install Stack with `$ sudo apt install haskell-platform`.

```console
$ cabal update
$ cabal install Jikka
```

## Documents

for users:

- [docs/language.md](https://github.com/kmyk/Jikka/blob/master/docs/language.md)
  - [docs/language.ja.md](https://github.com/kmyk/Jikka/blob/master/docs/language.ja.md) (Japanese translation)
- [docs/optimization.md](https://github.com/kmyk/Jikka/blob/master/docs/language.md)
- [docs/optimization.ja.md](https://github.com/kmyk/Jikka/blob/master/docs/language.md) (Japanese translation)
- [examples/](https://github.com/kmyk/Jikka/blob/master/examples)
- [CHANGELOG.md](https://github.com/kmyk/Jikka/blob/master/CHANGELOG.md)
- a blog article [競技プログラミングの問題を自動で解きたい - うさぎ小屋](https://kimiyuki.net/blog/2020/12/09/automated-solvers-of-competitive-programming/) (Japanese)

for developpers:

- [CONTRIBUTING.md](https://github.com/kmyk/Jikka/blob/master/CONTRIBUTING.md)
  - [CONTRIBUTING.ja.md](https://github.com/kmyk/Jikka/blob/master/CONTRIBUTING.ja.md) (Japanese translation)
- [docs/DESIGN.md](https://github.com/kmyk/Jikka/blob/master/docs/DESIGN.md) (Japanese)
- [docs/how-it-works.pdf](https://github.com/kmyk/Jikka/blob/master/docs/how-it-works.pdf) (Japanese)
- [Haddock](https://hackage.haskell.org/package/Jikka)
  - [Haddock (master)](https://kmyk.github.io/Jikka/)

## Examples

### `examples/fact.py` (`v5.0.5.0`)

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

### `examples/static_range_sum.py` (`v5.0.10.0`)

Input, O(N^2):

```python
# https://judge.yosupo.jp/problem/static_range_sum

from typing import *

def solve(n: int, q: int, a: List[int], l: List[int], r: List[int]) -> List[int]:
    ans = [-1 for _ in range(q)]
    for i in range(q):
        ans[i] = sum(a[l[i]:r[i]])
    return ans

def main() -> None:
    n, q = map(int, input().split())
    a = list(map(int, input().split()))
    assert len(a) == n
    l = list(range(q))
    r = list(range(q))
    for i in range(q):
        l[i], r[i] = map(int, input().split())
    ans = solve(n, q, a, l, r)
    for i in range(q):
        print(ans[i])

if __name__ == '__main__':
    main()
```

Output, O(N):

```c++
#include <algorithm>
#include <cstdint>
#include <functional>
#include <iostream>
#include <numeric>
#include <string>
#include <tuple>
#include <vector>
std::vector<int64_t> solve(int64_t n_0, int64_t q_1, std::vector<int64_t> a_2,
                           std::vector<int64_t> l_3, std::vector<int64_t> r_4) {
  std::vector<int64_t> x6 = std::vector<int64_t>(a_2.size() + 1);
  x6[0] = 0;
  for (int32_t i7 = 0; i7 < int32_t(a_2.size()); ++i7) {
    x6[(i7 + 1)] = x6[i7] + a_2[i7];
  }
  std::vector<int64_t> x5 = x6;
  std::vector<int64_t> x10 = std::vector<int64_t>(q_1);
  for (int32_t i11 = 0; i11 < int32_t(q_1); ++i11) {
    x10[i11] = -x5[l_3[i11]] + x5[r_4[i11]];
  }
  return x10;
}
int main() {
  int64_t n_13 = -1;
  int64_t q_14 = -1;
  std::cin >> n_13;
  std::vector<int64_t> a_15 = std::vector<int64_t>(n_13, -1);
  std::cin >> q_14;
  std::vector<int64_t> l_16 = std::vector<int64_t>(q_14, -1);
  std::vector<int64_t> r_17 = std::vector<int64_t>(q_14, -1);
  for (int32_t i18 = 0; i18 < n_13; ++i18) {
    std::cin >> a_15[i18];
  }
  for (int32_t i_19 = 0; i_19 < q_14; ++i_19) {
    std::cin >> l_16[i_19];
    std::cin >> r_17[i_19];
  }
  for (int32_t i_20 = 0; i_20 < q_14; ++i_20) {
  }
  auto ans_21 = solve(n_13, q_14, a_15, l_16, r_17);
  for (int32_t i_22 = 0; i_22 < q_14; ++i_22) {
  }
  for (int32_t i_23 = 0; i_23 < q_14; ++i_23) {
    std::cout << ans_21[i_23] << ' ';
    std::cout << '\n' << ' ';
  }
}
```

## License

Appache License 2.0
