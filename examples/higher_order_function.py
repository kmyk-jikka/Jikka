from typing import *

def repeat(f: Callable[[int], int], k: int) -> Callable[[int], int]:
    g = lambda x: x
    for _ in range(k):
        g = (lambda g: lambda x: f(g(x)))(g)
    return g

def solve(n: int) -> int:
    return repeat(lambda x: x + n, n)(n)
