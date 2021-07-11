#!/usr/bin/env python3
from typing import *

def solve(n: int, h: List[int]) -> int:
    dp = [-1 for _ in range(n)]
    dp[0] = 0
    dp[1] = abs(h[1] - h[0])
    for i in range(2, n):
        dp[i] = min(dp[i - 1] + abs(h[i] - h[i - 1]), dp[i - 2] + abs(h[i] - h[i - 2]))
    return dp[n - 1]

def main() -> None:
    n = int(input())
    _, *h = map(int, input().split())
    print(solve(n, h))

if __name__ == '__main__':
    main()
