# https://atcoder.jp/contests/abc206/tasks/abc206_b

from typing import *

def solve(n: int) -> int:
    c = 0
    ans = 0
    flag = True
    for i in range(100000): # (10^5)^2 > 10^9
        c += i
        if c >= n and flag:
            ans = i
            flag = False
    return ans

def main() -> None:
    n = int(input())
    ans = solve(n)
    print(ans)

if __name__ == '__main__':
    main()
