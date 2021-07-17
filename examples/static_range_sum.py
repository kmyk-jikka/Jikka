# https://judge.yosupo.jp/problem/static_range_sum

from typing import *

def solve(n: int, q: int, a: List[int], l: List[int], r: List[int]) -> List[int]:
    ans = [-1 for _ in range(q)]
    for i in range(q):
        ans[i] = sum(a[l[i]:r[i]])
    return ans

def main() -> None:
    n, q = map(int, input().split())
    a = list(map(int, input().split()))
    assert len(a) == n
    l = list(range(q))
    r = list(range(q))
    for i in range(q):
        l[i], r[i] = map(int, input().split())
    ans = solve(n, q, a, l, r)
    for i in range(q):
        print(ans[i])

if __name__ == '__main__':
    main()
