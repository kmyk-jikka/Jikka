# https://atcoder.jp/contests/abc208/tasks/abc208_b

from typing import *

def solve(p: int) -> int:
    e = 1
    cs = []
    ans = 0
    for i in range(10):
        e *= i+1
        cs.append(e)
    for c in reversed(cs):
        ans += p//c
        p -= p//c * c
    return ans

def main() -> None:
    p = int(input())
    ans = solve(p)
    print(ans)

if __name__ == '__main__':
    main()
