def solve(n: int) -> int:
    ans = 1
    for i in range(n):
        ans *= i + 1
    return ans % 998244353
