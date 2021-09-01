# https://judge.kimiyuki.net/problem/sum-sum-plus-two
from typing import *


def solve(a: List[int], b: List[int]) -> int:
    ans = 0
    for a_i in a:
        for b_j in b:
            ans += a_i - b_j
    return ans


def main() -> None:
    n, m = map(int, input().split())
    a = list(map(int, input().split()))
    assert len(a) == n
    b = list(map(int, input().split()))
    assert len(b) == m
    ans = solve(a, b)
    print(ans)


if __name__ == "__main__":
    main()
