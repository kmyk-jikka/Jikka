# https://atcoder.jp/contests/dp/tasks/dp_w
import math
from typing import *

def solve(n: int, m: int, l: List[int], r: List[int], a: List[int]) -> int:
    assert 1 <= n <= 2 * 10 ** 5
    assert 1 <= m <= 2 * 10 ** 5
    assert len(l) == m
    assert len(r) == m
    assert len(a) == m
    assert all(1 <= l[i] <= r[i] <= n for i in range(m))
    assert all(abs(a_i) <= 10 ** 9 for a_i in a)

    dp = [-math.inf for _ in range(n + 1)]
    dp[0] = 0
    for x in range(1, n + 1):
        for y in range(x):
            b = 0
            for i in range(m):
                if y < l[i] and l[i] <= x <= r[i]:
                    b += a[i]
            dp[x] = min(dp[x], b)
    return dp[n]
