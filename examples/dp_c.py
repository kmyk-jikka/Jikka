# https://atcoder.jp/contests/dp/tasks/dp_c
from typing import *

def solve(n: int, a: List[int], b: List[int], c: List[int]) -> int:
    assert 1 <= n <= 10 ** 5
    assert len(a) == n
    assert len(b) == n
    assert len(c) == n
    assert all(1 <= a_i <= 10 ** 4 for a_i in a)
    assert all(1 <= b_i <= 10 ** 4 for b_i in b)
    assert all(1 <= c_i <= 10 ** 4 for c_i in c)

    dp_a = [-1 for _ in range(n + 1)]
    dp_b = [-1 for _ in range(n + 1)]
    dp_c = [-1 for _ in range(n + 1)]
    dp_a[0] = 0
    dp_b[0] = 0
    dp_c[0] = 0
    for i in range(n):
        dp_a[i + 1] = a[i] + max(dp_b[i], dp_c[i])
        dp_b[i + 1] = b[i] + max(dp_c[i], dp_a[i])
        dp_c[i + 1] = c[i] + max(dp_a[i], dp_b[i])
    return max([dp_a[n], dp_b[n], dp_c[n]])

def main() -> None:
    n = int(input())
    a = list(range(n))
    b = list(range(n))
    c = list(range(n))
    for i in range(n):
        a[i], b[i], c[i] = map(int, input().split())
    ans = solve(n, a, b, c)
    print(ans)

if __name__ == '__main__':
    main()
