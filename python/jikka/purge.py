import functools
import operator
from math import gcd
from typing import *

nat = int

class _Interval:
    def __getitem__(self, args: Tuple[int, int]) -> type:
        l, r = args
        if l > r:
            raise ValueError('l must be greater than r for Interval[l, r]')
        return int
Interval = _Interval()

class _Array:
    def __getitem__(self, args: Tuple[type, int]) -> type:
        t, n = args
        return List[t]
Array = _Array()

def floordiv(x: int, y: int) -> int:
    if y == 0:
        raise ZeroDivisionError('integer division by zero')
    return x // y

def ceildiv(x: int, y: int) -> int:
    if y == 0:
        raise ZeroDivisionError('integer division by zero')
    return (x + y - 1) // y

def product(xs: List[int]) -> int:
    return functools.reduce(operator.mul, xs, 1)

def argmin(xs: List[int]) -> nat:
    if not xs:
        raise ValueError('argmin() arg is an empty sequence')
    return min(range(len(xs)), key=lambda i: (xs[i], i))

def argmax(xs: List[int]) -> nat:
    if not xs:
        raise ValueError('argmax() arg is an empty sequence')
    return max(range(len(xs)), key=lambda i: (xs[i], i))

def fact(n: nat) -> nat:
    if n < 0:
        raise ValueError('argument must be non-negative')
    return functools.reduce(operator.mul, range(1, n + 1), 1)

def choose(n: nat, r: nat) -> nat:
    if n < 0:
        raise ValueError('n must be non-negative')
    if r < 0:
        raise ValueError('r must be non-negative')
    if r > n:
        raise ValueError('r must be less-than or equal-to n')
    return fact(n) // fact(n - r) // fact(r)

def permute(n: nat, r: nat) -> nat:
    if n < 0:
        raise ValueError('n must be non-negative')
    if r < 0:
        raise ValueError('r must be non-negative')
    if r > n:
        raise ValueError('r must be less-than or equal-to n')
    return fact(n) // fact(n - r)

def multichoose(n: nat, r: nat) -> nat:
    if n < 0:
        raise ValueError('n must be non-negative')
    if r < 0:
        raise ValueError('r must be non-negative')
    if r > n:
        raise ValueError('r must be less-than or equal-to n')
    if n == r == 0:
        return 1
    return choose(n + r - 1, r)

def lcm(x: int, y: int) -> int:
    return x * y // gcd(x, y)

def modinv(x: int, mod: nat) -> nat:
    if mod <= 0:
        raise ValueError('mod must be positive')
    return pow(x, -1, mod)
