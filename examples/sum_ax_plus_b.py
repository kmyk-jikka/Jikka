# https://judge.kimiyuki.net/problem/sum-ax-plus-b
def solve(a: int, b: int, n: int) -> int:
    y = 0
    for x in range(n):
        y += a * x + b
    return y % 998244353
