#!/usr/bin/env python3
from typing import *


def solve1(n: int, a: List[int]) -> List[int]:
    c = [0 for _ in range(2 * n)]
    for j in range(n):
        c[j + 1] += a[j]
        if j + n + 1 < 2 * n:
            c[j + n + 1] -= a[j]
    for k in range(2 * n - 1):
        c[k + 1] += c[k]
    for j in range(n):
        if j + n + 1 < 2 * n:
            c[j + n + 1] -= n * a[j]
    for k in range(2 * n - 1):
        c[k + 1] += c[k]
    return c


def solve(n: int, a: List[int], b: List[int]) -> List[int]:
    c1 = solve1(n, a)
    c2 = solve1(n, b)
    c = list(map(lambda x, y: x + y, c1, c2))
    return c


def main():
    n = int(input())
    a = list(map(int, input().split()))
    b = list(map(int, input().split()))
    ans = solve(n, a, b)
    print(*ans)


if __name__ == '__main__':
    main()
