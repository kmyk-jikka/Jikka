# https://atcoder.jp/contests/abc127/tasks/abc127_e

MOD: int = 10 ** 9 + 7

def solve(h: int, w: int, k: int) -> int:
    ans = 0
    for y1 in range(h):
        for x1 in range(w):
            for y2 in range(h):
                for x2 in range(w):
                    if (y1, x1) < (y2, x2):
                        ans += choose(h * w - 2, k - 2) * (abs(y2 - y1) + abs(x2 - x1))
    return ans % MOD

def main() -> None:
    n, m, k = map(int, input().split())
    ans = solve(n, m, k)
    print(ans)

if __name__ == '__main__':
    main()
