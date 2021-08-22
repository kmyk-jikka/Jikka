# See https://github.com/kmyk/Jikka/issues/173

def solve(n: int, k: int) -> int:
    for _ in range(k):
        n = n % 3
    return n

def main() -> None:
    n, k = map(int, input().split())
    ans = solve(n, k)
    print(ans)

if __name__ == '__main__':
    main()
