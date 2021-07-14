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
            b = max(b, dp[j])
        dp[h[i] - 1] = b + a[i]
    return max(dp)

def main() -> None:
    n = int(input())
    h = list(map(int, input().split()))
    assert len(h) == n
    a = list(map(int, input().split()))
    assert len(a) == n
    ans = solve(n, h, a)
    print(ans)

if __name__ == '__main__':
    main()
