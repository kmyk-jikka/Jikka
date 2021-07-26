#!/usr/bin/env python3
import argparse
import random


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('-n', type=int, default=random.randint(2, 10**5))
    args = parser.parse_args()

    h = list(range(1, args.n + 1))
    random.shuffle(h)
    a = [random.randint(1, 10**9) for _ in range(args.n)]
    print(args.n)
    print(*h)
    print(*a)


if __name__ == '__main__':
    main()
