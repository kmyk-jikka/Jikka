from typing import *


def solve(n: int, q: int, a: List[int], l: List[int], r: List[int]) -> List[int]:
    b = [0] * (n + 1)
    for i in range(n):
        b[i + 1] = b[i] + a[i]
    ans = [-1] * q
    for j in range(q):
        ans[j] = b[r[j]] - b[l[j]]
    return ans


def main() -> None:
    n = int(input())
    q = int(input())
    a = list(map(int, input().split()))
    l = list(range(q))
    r = list(range(q))
    for i in range(q):
        l[i], r[i] = map(int, input().split())
    ans = solve(n, q, a, l, r)
    for i in range(q):
        print(ans[i])


if __name__ == '__main__':
    main()
