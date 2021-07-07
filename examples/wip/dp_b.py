# https://atcoder.jp/contests/dp/tasks/dp_b
import math
from typing import *

def solve(n: int, k: int, h: List[int]) -> int:
    assert 2 <= n <= 10 ** 5
    assert 1 <= k <= 100
    assert len(h) == n
    assert all(1 <= h_i <= 10 ** 4 for h_i in h)

    dp = [math.inf for _ in range(n)]
    dp[0] = 0
    for i in range(1, n):
        for j in range(max(0, i - k), i):
            dp[i] = min(dp[i], dp[j] + abs(h[i] - h[j]))
    return dp[n - 1]
