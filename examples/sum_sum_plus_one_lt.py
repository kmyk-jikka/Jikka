# https://judge.kimiyuki.net/problem/sum-sum-plus-one-lt
from typing import *


def solve(a: List[int]) -> int:
    n = len(a)
    ans = 0
    for i in range(n):
        for j in range(i + 1, n):
            ans += a[i] - a[j]
    return ans


def main() -> None:
    n = int(input())
    a = list(map(int, input().split()))
    assert len(a) == n
    ans = solve(a)
    print(ans)


if __name__ == "__main__":
    main()
