def f(n: int) -> int:
    a = 0
    b = 1
    for _ in range(n):
        c = a + b
        a = b
        b = c
    return a % 1000000007

def solve(n: int) -> int:
    return f(n)
