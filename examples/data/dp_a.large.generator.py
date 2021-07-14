#!/usr/bin/env python3
import random

def main() -> None:
    # n = random.randint(2, 10 ** 5)
    n = random.randint(2, 10 ** 3)
    h = [random.randint(1, 10 ** 4) for _ in range(n)]
    print(n)
    print(*h)

if __name__ == '__main__':
    main()
