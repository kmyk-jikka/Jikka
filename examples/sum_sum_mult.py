# https://judge.kimiyuki.net/problem/sum-sum-square
from typing import *


def solve(a: List[int]) -> int:
    ans = 0
    for a_i in a:
        for a_j in a:
            ans += a_i * a_j
    return ans % 998244353


def main() -> None:
    n = int(input())
    a = list(map(int, input().split()))
    assert len(a) == n
    ans = solve(a)
    print(ans)


if __name__ == "__main__":
    main()
