from typing import *


def solve(n: int) -> int:
    a = 0
    b = 1
    lis = []
    for i in range(n):
        lis.append(0)
    for i in lis:
        c = a + b + i
        a = b
        b = c
    return a % 1000000007


def main():
    n = int(input())
    ans = solve(n)
    print(ans)


if __name__ == "__main__":
    main()
