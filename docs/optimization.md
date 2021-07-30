# Available Optimizations

(このドキュメントの日本語バージョン: [optimization.ja.md](https://github.com/kmyk/Jikka/blob/master/docs/optimization.ja.md))

This document introduces some important optimizations that Jikka does.

## Matrix Exponentiation

For a commutative ring R, a loop which has a linear or affine reccurence formula is converted to a matrix exponentiation.
The integer ring ℤ and quotient rings ℤ/nℤ are available for this R.
That is, a loop which is equivalent to the below is processed with O(log n).

```python
for _ in range(n):
    y1 = f11 * x1 + f12 * x2 + ... + f1k * xk + c1
    y2 = f21 * x1 + f22 * x2 + ... + f2k * xk + c2
    ...
    yk = fk1 * x1 + fk2 * x2 + ... + fkk * xk + ck
    x1, x2, ..., xk = y1, y2, ..., yk
```

For example, [examples/fib.py](https://github.com/kmyk/Jikka/blob/master/examples/fib.py) runs in O(log n).

- source code [src/Jikka/Core/Convert/MatrixExponentiation.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/MatrixExponentiation.hs)
- Haddock [Jikka.Core.Convert.MatrixExponentiation](https://hackage.haskell.org/package/Jikka/docs/Jikka-Core-Convert-MatrixExponentiation.html)

## Cumulative Sum

An accumulation of a list of elements of a semigroup S is converted to a cumulative sum.

For example, code like below is processed with O(n).

```python
b = 0
for i in range(n):
    b += sum(a[:i])
```

For example, [examples/static_range_sum.py](https://github.com/kmyk/Jikka/blob/master/examples/static_range_sum.py) runs in O(n + q).

- source code [src/Jikka/Core/Convert/CumulativeSum.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/CumulativeSum.hs)
- Haddock [Jikka.Core.Convert.CumulativeSum](https://hackage.haskell.org/package/Jikka/docs/Jikka-Core-Convert-CumulativeSum.html)

## Segment Tree

An accumulation of a list of elements of a semigroup S is converted to a segment tree if required.

For example, code like below is processed with O(n log n).

```python
b = 0
for i in range(n):
    a[c[i]] += d[i]
    b += sum(a[:i])
```

For example, [examples/dp_q.py](https://github.com/kmyk/Jikka/blob/master/examples/dp_q.py) runs in O(n log n).

- source code [src/Jikka/Core/Convert/SegmentTree.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/SegmentTree.hs)
- Haddock [Jikka.Core.Convert.SegmentTree](https://hackage.haskell.org/package/Jikka/docs/Jikka-Core-Convert-SegmentTree.html)

## Convex Hull Trick

A loop to get the minimum or maximum of values of a set of linear of affine functions is converted with convex hull trick.
That is, a loop which is equivalent to the below is processed with O(n log n).

```python
for i in range(n):
    y = k
    for j in range(n):
        y = min(y, a(j) * x(i) + b(j) + c(i))
    ...
```

For example, [examples/dp_z.py](https://github.com/kmyk/Jikka/blob/master/examples/dp_z.py) runs in O(n log n).

- source code [src/Jikka/Core/Convert/ConvexHullTrick.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/ConvexHullTrick.hs)
- Haddock [Jikka.Core.Convert.ConvexHullTrick](https://hackage.haskell.org/package/Jikka/docs/Jikka-Core-Convert-ConvexHullTrick.html)
