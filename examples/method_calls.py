def solve(a: int, b: int, c: int) -> int:
    xs = [a, b, c, 1]
    return xs.count(0) + xs.index(1)
