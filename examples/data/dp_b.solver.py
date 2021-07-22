from typing import *

INF = 10 ** 18

def solve(n: int, k: int, h: List[int]) -> int:
    dp = [INF for _ in range(n)]
    dp[0] = 0
    for i in range(1, n):
        for j in range(max(0, i - k), i):
            dp[i] = min(dp[i], dp[j] + abs(h[i] - h[j]))
    return dp[n - 1]

def main() -> None:
    n, k = map(int, input().split())
    h = list(map(int, input().split()))
    ans = solve(n, k, h)
    print(ans)

if __name__ == '__main__':
    main()
