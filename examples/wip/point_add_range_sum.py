# https://judge.yosupo.jp/problem/point_add_range_sum

from typing import *

def solve(n: int, q: int, a: List[int], t: List[int], args1: List[int], args2: List[int]) -> List[int]:
    ans = []
    for i in range(q):
        if t[i] == 0:
            p = args1[i]
            x = args2[i]
            a[p] += x
        else:
            l = args1[i]
            r = args2[i]
            ans.append(sum(a[l:r]))
    return ans
