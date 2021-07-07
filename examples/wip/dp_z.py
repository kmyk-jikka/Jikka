# https://atcoder.jp/contests/dp/tasks/dp_z
import math
from typing import *

def solve(n: int, c: int, h: List[int]) -> int:
    assert 2 <= n <= 10 ** 5
    assert 1 <= c <= 10 ** 12
    assert len(h) == n
    assert all(1 <= h_i <= 10 ** 6 for h_i in h)

    dp = [math.inf for _ in range(n)]
    dp[0] = 0
    for i in range(n):
        for j in range(i + 1, n):
            dp[j] = min(dp[j], (h[i] - h[j]) ** 2 + c)
    return dp[n - 1]
