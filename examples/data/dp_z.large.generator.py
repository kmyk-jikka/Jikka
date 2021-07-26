#!/usr/bin/env python3
import argparse
import random


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('-n', type=int, default=random.randint(2, 2 * 10**5))
    args = parser.parse_args()

    c = random.randint(1, 10**12)
    h = list(range(1, 10**6))
    random.shuffle(h)
    h = h[:args.n]
    print(args.n, c)
    print(*h)


if __name__ == '__main__':
    main()
