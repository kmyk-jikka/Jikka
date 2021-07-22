#!/usr/bin/env python3
import argparse
import random


def main():
    parser = argparse.ArgumentParser()
    # parser.add_argument('-n', type=int, default=random.randint(1, 2 * 10 ** 5))
    parser.add_argument('-n', type=int, default=random.randint(1, 2 * 100))
    args = parser.parse_args()

    a = [random.randint(0, 2 * 10**5) for _ in range(args.n)]
    b = [random.randint(0, 2 * 10**5) for _ in range(args.n)]
    print(args.n)
    print(*a)
    print(*b)


if __name__ == "__main__":
    main()
