def solve() -> int:
    x = 0
    f = lambda: x
    x = 1  # err
    return f()
