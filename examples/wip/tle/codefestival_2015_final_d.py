# https://atcoder.jp/contests/code-festival-2015-final-open/tasks/codefestival_2015_final_d
from typing import *

def solve(n: int, s: List[int], t: List[int]) -> int:
    assert 2 <= n <= 10 ** 5
    assert len(s) == n
    assert len(t) == n
    ans = n
    for i in range(n):
        ans_i = max(sum((1 if s[j] <= x < t[j] else 0) for j in range(n) if j != i) for x in range(max(t)))
        ans = min(ans, ans_i)
    return ans
