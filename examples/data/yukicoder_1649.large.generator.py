#!/usr/bin/env python3
import argparse
import random


def main():
    parser = argparse.ArgumentParser()
    # parser.add_argument('-n', type=int, default=random.randint(1, 2 * 10 ** 5))
    parser.add_argument('-n', type=int, default=random.randint(1, 2 * 100))
    args = parser.parse_args()

    N = args.n
    x = [None for _ in range(N)]
    y = [None for _ in range(N)]
    used = set()
    for i in range(N):
        while True:
            x[i] = random.randint(1, 10**9)
            y[i] = random.randint(1, 10**9)
            if (x[i], y[i]) not in used:
                used.add((x[i], y[i]))
                break
    print(N)
    for i in range(N):
        print(x[i], y[i])


if __name__ == "__main__":
    main()
