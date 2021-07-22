#!/usr/bin/env python3
import argparse
import random


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('-n', type=int, default=random.randint(2, 10**5))
    args = parser.parse_args()

    k = random.randint(1, 100)
    h = [random.randint(1, 10**4) for _ in range(args.n)]
    print(args.n, k)
    print(*h)


if __name__ == '__main__':
    main()
