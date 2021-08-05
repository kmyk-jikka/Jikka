# https://atcoder.jp/contests/abc200/tasks/abc200_b

from typing import *

def solve(n: int, k: int) -> int:
    for _ in range(k):
        if n % 200 == 0:
            n //= 200
        else:
            n = n*1000 + 200
    return n

def main() -> None:
    n, k = map(int, input().split())
    ans = solve(n, k)
    print(ans)

if __name__ == '__main__':
    main()
