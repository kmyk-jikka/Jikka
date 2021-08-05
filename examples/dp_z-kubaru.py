# https://atcoder.jp/contests/dp/tasks/dp_z
from typing import *

INF = 10 ** 18

def solve(n: int, c: int, h: List[int]) -> int:
    assert 2 <= n <= 2 * 10 ** 5
    assert 1 <= c <= 10 ** 12
    assert len(h) == n
    assert all(1 <= h_i <= 10 ** 6 for h_i in h)

    dp = [INF for _ in range(n)]
    dp[0] = 0
    for i in range(n):
        for j in range(i + 1, n):
            dp[j] = min(dp[j], dp[i] + (h[i] - h[j]) ** 2 + c)
    return dp[n - 1]

def main() -> None:
    n, c = map(int, input().split())
    h = list(map(int, input().split()))
    assert len(h) == n
    ans = solve(n, c, h)
    print(ans)

if __name__ == '__main__':
    main()
