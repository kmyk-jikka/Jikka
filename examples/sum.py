def solve(a: int, b: int, n: int) -> int:
    y = 0
    for x in range(n):
        y += a * x + b
    return y
