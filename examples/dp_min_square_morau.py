# https://judge.kimiyuki.net/problem/dp-min-square

INF = 10 ** 18

def solve(a: List[int]) -> int:
    n = len(a)
    dp = [INF for _ in range(n)]
    dp[0] = 0
    for i in range(1, n):
        for j in range(i):
            dp[i] = min(dp[i], dp[j] + (a[i] - a[j]) ** 2)
    return dp[n - 1]
