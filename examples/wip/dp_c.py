# https://atcoder.jp/contests/dp/tasks/dp_c
from typing import *

def solve(n: int, a: List[int], b: List[int], c: List[int]) -> int:
    assert 1 <= n <= 10 ** 5
    assert len(a) == n
    assert len(b) == n
    assert len(c) == n
    assert all(1 <= a_i <= 10 ** 4 for a_i in a)
    assert all(1 <= b_i <= 10 ** 4 for b_i in b)
    assert all(1 <= c_i <= 10 ** 4 for c_i in c)

    dp = [[-1, -1, -1] for _ in range(n + 1)]
    dp[0] = [0, 0, 0]
    for i in range(n):
        dp[i + 1][0] = a[i] + max(dp[i][1], dp[i][2])
        dp[i + 1][0] = b[i] + max(dp[i][2], dp[i][0])
        dp[i + 1][0] = c[i] + max(dp[i][0], dp[i][1])
    return max(dp[n])
