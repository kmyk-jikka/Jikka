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

def main() -> None:
    n, q = map(int, input().split())
    a = list(map(int, input().split()))
    assert len(a) == n
    t = list(range(q))
    args1 = list(range(q))
    args2 = list(range(q))
    for i in range(q):
        t[i], args1[i], args2[i] = map(int, input().split())
    ans = solve(n, q, a, t, args1, args2)
    for i in range(len(ans)):
        print(ans[i])

if __name__ == '__main__':
    main()
