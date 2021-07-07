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
            b = min(b, h[j])
        dp[h[i] - 1] = b + a[i]
    return dp[n - 1]
