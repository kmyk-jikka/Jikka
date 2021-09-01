# https://judge.kimiyuki.net/problem/dp-min-square

INF = 10 ** 18

def solve(a: List[int]) -> int:
    n = len(a)
    dp = [INF for _ in range(n)]
    dp[0] = 0
    for j in range(n):
        for i in range(j + 1, n):
            dp[i] = min(dp[i], dp[j] + (a[i] - a[j]) ** 2)
    return dp[n - 1]
