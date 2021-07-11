#!/usr/bin/env python3
import argparse
import random

def main() -> None:
    parser = argparse.ArgumentParser()
    # parser.add_argument('-n', type=int, default=random.randint(2, 200000))
    parser.add_argument('-n', type=int, default=random.randint(2, 1000))
    args = parser.parse_args()

    n = args.n
    a = [random.randint(1, 200000) for _ in range(n)]
    print(n)
    print(len(a), *a)

if __name__ == '__main__':
    main()
