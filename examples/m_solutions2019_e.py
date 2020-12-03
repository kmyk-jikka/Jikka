# https://atcoder.jp/contests/m-solutions2019/tasks/m_solutions2019_e
from jikka.compat import *

MOD: int = 1000003

def solve1(x: Interval["0", "MOD - 1"], d: Interval["0", "MOD - 1"], n: Interval["0", "1000000000"]) -> Interval["0", "MOD - 1"]:
    return sum([x + i * d for i in range(n)]) % MOD

def solve(Q: Interval["1", "100000"], x: Array[Interval["0", "MOD - 1"], "Q"], d: Array[Interval["0", "MOD - 1"], "Q"], n: Array[Interval["1", "1000000000"], "Q"]) -> Array[Interval["0", "MOD - 1"], "Q"]:
    ans = [None for _ in range(Q)]
    for i in range(Q):
        ans[i] = solve1(x[i], d[i], n[i])
    return ans
