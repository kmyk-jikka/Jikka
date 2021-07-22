#!/usr/bin/env python3
from typing import *

def solve(n: int, a: List[int]) -> List[int]:
    l = [-1] * (n + 1)
    r = [-1] * (n + 1)
    for i in range(n):
        l[i + 1] = max(l[i], a[i])
    for i in reversed(range(n)):
        r[i] = max(r[i + 1], a[i])
    ans = [-1] * n
    for i in range(n):
        ans[i] = max(l[i], r[i + 1])
    return ans

def main() -> None:
    n = int(input())
    _, *a = map(int, input().split())
    ans = solve(n, a)
    print(len(ans), *ans)

if __name__ == '__main__':
    main()
