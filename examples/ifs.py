def solve(n: int) -> int:
    a = 0
    if n & 1 != 0:
        a += 1
    else:
        a += 3
    if n & 2 != 0:
        a *= 6
    if n & 4 != 0:
        a -= 3
    else:
        a = 3 * a + 2
    if n & 8 != 0:
        a += 1
    else:
        a -= 1
    if n & 16 != 0:
        a += 2
    else:
        a -= 2
    if n & 32 != 0:
        a += 3
    else:
        a -= 3
    if n & 64 != 0:
        a += 4
    else:
        a -= 4
    if n & 128 != 0:
        a += 5
    else:
        a -= 5
    return a
