def f(n: int) -> int:
    a = 0
    b = 1
    for _ in range(n):
        c = (a + b) % 1000000007
        a = b
        b = c
    return a

def solve(n: int) -> int:
    return f(n)
