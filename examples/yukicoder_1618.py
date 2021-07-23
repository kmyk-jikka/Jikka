# https://yukicoder.me/problems/no/1618
from typing import *


def solve(n: int, a: List[int], b: List[int]) -> List[int]:
    c = [0 for _ in range(2 * n)]
    for i in range(n):
        for j in range(n):
            c[i + j + 1] += (i + 1) * a[j] + (j + 1) * b[i]
    return c


def main():
    n = int(input())
    a = list(map(int, input().split()))
    assert len(a) == n
    b = list(map(int, input().split()))
    assert len(b) == n
    ans = solve(n, a, b)
    print(*ans)


if __name__ == '__main__':
    main()
