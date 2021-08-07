# https://atcoder.jp/contests/abc204/tasks/abc204_b

from typing import *

def solve(n: int, a: List[int]) -> int:
    ans = 0
    for e in a:
        if e > 10:
            ans += e - 10
    return ans

def main() -> None:
    n = int(input())
    a = list(map(int, input().split()))
    assert len(a) == n
    ans = solve(n, a)
    print(ans)

if __name__ == '__main__':
    main()
