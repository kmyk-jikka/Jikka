#!/usr/bin/env python3
from typing import *

def solve(n: int, a: List[int], b: List[int], c: List[int]) -> int:
    dp = [[-1, -1, -1] for _ in range(n + 1)]
    dp[0] = [0, 0, 0]
    for i in range(n):
        dp[i + 1][0] = a[i] + max(dp[i][1], dp[i][2])
        dp[i + 1][1] = b[i] + max(dp[i][2], dp[i][0])
        dp[i + 1][2] = c[i] + max(dp[i][0], dp[i][1])
    return max(dp[n])

def main() -> None:
    n = int(input())
    a = list(range(n))
    b = list(range(n))
    c = list(range(n))
    for i in range(n):
        a[i], b[i], c[i] = map(int, input().split())
    ans = solve(n, a, b, c)
    print(ans)

if __name__ == '__main__':
    main()
