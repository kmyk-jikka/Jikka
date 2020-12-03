# https://atcoder.jp/contests/m-solutions2019/tasks/m_solutions2019_e
from typing import *

MOD: int = 1000003

def solve1(x: int, d: int, n: int) -> int:
    assert 0 <= x < MOD
    assert 0 <= d < MOD
    assert 0 <= n < 10 ** 9

    return sum([x + i * d for i in range(n)]) % MOD

def solve(Q: int, x: List[int], d: List[int], n: List[int]) -> List[int]:
    assert 1 <= Q <= 100000
    assert len(x) == Q
    assert all(0 <= x_i < MOD for x_i in x)
    assert len(d) == Q
    assert all(0 <= d_i < MOD for d_i in d)
    assert len(n) == Q
    assert all(0 <= n_i < MOD for n_i in n)

    ans = [None for _ in range(Q)]
    for i in range(Q):
        ans[i] = solve1(x[i], d[i], n[i])
    return ans
