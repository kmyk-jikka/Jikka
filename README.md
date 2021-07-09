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

``` console
$ stack run convert PYTHON_FILE
```

[Stack](https://www.haskellstack.org/) is required. If you are using Ubuntu, you can install Stack with `$ sudo apt install haskell-stack`.


## Documents

for users:

-   [docs/language.md](https://github.com/kmyk/Jikka/blob/master/docs/language.md)
    -   [docs/language.ja.md](https://github.com/kmyk/Jikka/blob/master/docs/language.ja.md) (Japanese translation)
-   [examples/](https://github.com/kmyk/Jikka/blob/master/examples)
-   [CHANGELOG.md](https://github.com/kmyk/Jikka/blob/master/CHANGELOG.md)
-   a blog article [競技プログラミングの問題を自動で解きたい - うさぎ小屋](https://kimiyuki.net/blog/2020/12/09/automated-solvers-of-competitive-programming/) (Japanese)

for developpers:

-   [CONTRIBUTING.md](https://github.com/kmyk/Jikka/blob/master/CONTRIBUTING.md)
    -   [CONTRIBUTING.ja.md](https://github.com/kmyk/Jikka/blob/master/CONTRIBUTING.ja.md) (Japanese translation)
-   [docs/DESIGN.md](https://github.com/kmyk/Jikka/blob/master/docs/DESIGN.md) (Japanese)
-   [docs/how-it-works.pdf](https://github.com/kmyk/Jikka/blob/master/docs/how-it-works.pdf) (Japanese)
-   [Haddock](https://kmyk.github.io/Jikka/)


## Examples (`v5.0.5.0`)

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


## License

Appache License 2.0
