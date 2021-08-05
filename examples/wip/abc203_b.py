# https://atcoder.jp/contests/abc203/tasks/abc203_b

from typing import *

def solve(n: int, k: int) -> int:
    a = []
    for i in range(1,n+1):
        for j in range(1,k+1):
            a.append(100*i + j)   
    ans = sum(a)
    return ans

def main() -> None:
    n, k = map(int, input().split())
    ans = solve(n, k)
    print(ans)

if __name__ == '__main__':
    main()
