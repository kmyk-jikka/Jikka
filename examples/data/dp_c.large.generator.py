#!/usr/bin/env python3
import argparse
import random


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('-n', type=int, default=random.randint(1, 10**5))
    args = parser.parse_args()

    a = [random.randint(1, 10**4) for _ in range(args.n)]
    b = [random.randint(1, 10**4) for _ in range(args.n)]
    c = [random.randint(1, 10**4) for _ in range(args.n)]
    print(args.n)
    for i in range(args.n):
        print(a[i], b[i], c[i])


if __name__ == '__main__':
    main()
