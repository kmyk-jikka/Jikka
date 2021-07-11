#!/usr/bin/env python3
import random

def main() -> None:
    n = random.randint(1, 5 * 10 ** 3)
    q = random.randint(1, 5 * 10 ** 3)
    a = [random.randint(1, 10 ** 9) for _  in range(n)]
    l = [random.randint(0, n - 1) for _  in range(q)]
    r = [random.randint(l[i] + 1, n) for i  in range(q)]
    print(n)
    print(q)
    print(len(a), *a)
    print(len(l), *l)
    print(len(r), *r)

if __name__ == '__main__':
    main()
