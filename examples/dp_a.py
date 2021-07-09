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
