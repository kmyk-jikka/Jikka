from typing import *

def solve(n: int, q: int, a: List[int], l: List[int], r: List[int]) -> None:
    b = [0] * (n + 1)
    for i in range(n):
        b[i + 1] = b[i] + a[i]
    ans = [-1] * q
    for j in range(q):
        ans[j] = b[r[j]] - b[l[j]]
    return ans

def main() -> None:
    n = int(input())
    q = int(input())
    _, *a = map(int, input().split())
    _, *l = map(int, input().split())
    _, *r = map(int, input().split())
    ans = solve(n, q, a, l, r)
    print(len(ans), *ans, sep='\n')

if __name__ == '__main__':
    main()
