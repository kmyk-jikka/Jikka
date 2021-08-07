# https://atcoder.jp/contests/abc207/tasks/abc207_b

# from typing import *

def solve(a: int, b: int, c: int, d: int) -> int:
    ans = -1
    if d*c - b > 0:
        ans = (a + d*c - b - 1) // (d*c - b) # ans = a /^ (d*c - b)
    return ans

def main() -> None:
    a, b, c, d = map(int, input().split())
    ans = solve(a, b, c, d)
    print(ans)

if __name__ == '__main__':
    main()
