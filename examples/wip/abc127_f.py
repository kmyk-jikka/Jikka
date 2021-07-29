# https://atcoder.jp/contests/abc127/tasks/abc127_f
from typing import *
import jikka

def solve(q: int, queries: List[List[int]]) -> List[Tuple[int, int]]:
    f = lambda x: 0
    ans = []
    for query in queries:
        if query[0] == 1:
            a = query[1]
            b = query[1]
            f = (lambda f: lambda x: f(x) + abs(x - a) + b)(f)
        else:
            assert query[0] == 2
            x = epsilon x: f(x) <= f(epsilon y: y) and x <= (epsilon z: f(z) <= f(epsilon y: y))
            ans.append((x, f(x)))
    return asn
