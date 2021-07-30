# Available Optimizations

(The English version of this document: [optimization.md](https://github.com/kmyk/Jikka/blob/master/docs/optimization.md))

このドキュメントでは Jikka が行う最適化のうちで主要なものを紹介します。

## Matrix Exponentiation

可換環 R に対しその線形あるいは affine な漸化式で書けるループは行列累乗に変換されます。
R としては整数環 ℤ あるいは剰余環 ℤ/nℤ が使えます。
つまり、以下と等価なループは O(log n) で処理されます。

```python
for _ in range(n):
    y1 = f11 * x1 + f12 * x2 + ... + f1k * xk + c1
    y2 = f21 * x1 + f22 * x2 + ... + f2k * xk + c2
    ...
    yk = fk1 * x1 + fk2 * x2 + ... + fkk * xk + ck
    x1, x2, ..., xk = y1, y2, ..., yk
```

たとえば [examples/fib.py](https://github.com/kmyk/Jikka/blob/master/examples/fib.py) は O(log n) です。

- source code [src/Jikka/Core/Convert/MatrixExponentiation.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/MatrixExponentiation.hs)
- Haddock [Jikka.Core.Convert.MatrixExponentiation](https://hackage.haskell.org/package/Jikka/docs/Jikka-Core-Convert-MatrixExponentiation.html)

## Cumulative Sum

半群 S の要素のリストの累積演算は累積和へと変換されます。

たとえば以下のようなコードは O(n) で処理されます。

```python
b = 0
for i in range(n):
    b += sum(a[:i])
```

たとえば [examples/static_range_sum.py](https://github.com/kmyk/Jikka/blob/master/examples/static_range_sum.py) は O(n + q) です。

- source code [src/Jikka/Core/Convert/CumulativeSum.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/CumulativeSum.hs)
- Haddock [Jikka.Core.Convert.CumulativeSum](https://hackage.haskell.org/package/Jikka/docs/Jikka-Core-Convert-CumulativeSum.html)

## Segment Tree

半群 S の要素のリストの累積演算は必要ならセグメント木に変換されます。

たとえば以下のようなコードは O(n log n) で処理されます。

```python
b = 0
for i in range(n):
    a[c[i]] += d[i]
    b += sum(a[:i])
```

たとえば [examples/dp_q.py](https://github.com/kmyk/Jikka/blob/master/examples/dp_q.py) は O(n log n) です。

- source code [src/Jikka/Core/Convert/SegmentTree.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/SegmentTree.hs)
- Haddock [Jikka.Core.Convert.SegmentTree](https://hackage.haskell.org/package/Jikka/docs/Jikka-Core-Convert-SegmentTree.html)

## Convex Hull Trick

一次関数の集合の値の最小値あるいは最大値を取得するループは convex hull trick を用いて変換されます。
つまり、以下と等価なループは O(n log n) で処理されます。

```python
for i in range(n):
    y = k
    for j in range(n):
        y = min(y, a(j) * x(i) + b(j) + c(i))
    ...
```

たとえば [examples/dp_z.py](https://github.com/kmyk/Jikka/blob/master/examples/dp_z.py) は O(n log n) です。

- source code [src/Jikka/Core/Convert/ConvexHullTrick.hs](https://github.com/kmyk/Jikka/blob/master/src/Jikka/Core/Convert/ConvexHullTrick.hs)
- Haddock [Jikka.Core.Convert.ConvexHullTrick](https://hackage.haskell.org/package/Jikka/docs/Jikka-Core-Convert-ConvexHullTrick.html)
