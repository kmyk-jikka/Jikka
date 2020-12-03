def f(n: int) -> int:
    if n == 0:
        return 1
    else:
        return n * f(n - 1)
