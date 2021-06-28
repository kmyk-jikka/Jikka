# https://atcoder.jp/contests/abc134/tasks/abc134_c
from typing import *

def solve(N: int, A: List[int]) -> List[int]:
    assert 2 <= N <= 200000
    assert len(A) == N
    assert all(0 <= A_i <= 200000 for A_i in A)

    ans = [-1 for _ in range(N)]
    for i in range(N):
        ans[i] = max((0 if j == i else A[j]) for j in range(N))
    return ans
