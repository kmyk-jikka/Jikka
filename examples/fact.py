def solve(n: int) -> int:
    if n == 0:
        return 1
    else:
        return n * solve(n - 1)
