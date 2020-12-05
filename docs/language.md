# Language Specification

## Overview

a very restricted subset of Python + additional builtin functions


## Syntax Overview

### Lexical Analysis

It's the almost same to Python.

### Grammer

It's highly restricted, but basically the same to Python.

### Expression

The followings are available:

-   literals
    -   `0`, `1`, `2`, ...
    -   `True`, `False`
-   list
    -   `[a, b, c, ...]`
    -   `[... for ... in ...]`
-   generator
    -   `(... for ... in ...)`
-   subscription
    -   `a[i]`
-   call
    -   `f(a, b, c, ...)`
    -   `f(... for ... in ...)`
-   operators
    -   See the "Standard Library" section.
-   conditional operator
    -   `... if ... eles ...`

The followings (and the other features) are unavailable:

-   list with stars
    -   `[x, *xs]`
-   comprehensions with filtering
    -   `... for ... in ... if ...`
-   slicing
    -   `a[l : r]`
-   attribute reference
    -   `foo.bar`
-   complicated calls
    -   `f(foo=a, bar=b)`
    -   `f(*args)`
-   lambdas
    -   `lambda x: ...`


### Simple statement

The followings are available:

-   assignment statement
    -   `x = 0`
    -   `a[y][x] = b`
    -   `a, b = b, a`
-   augmented assignment statement
    -   `x += 1`
-   annotated assignment statements
    -   `xs: List[List[int]] = []`
-   assert
    -   `assert 0 <= n`
-   pass
    -   `pass`
-   return
    -   `return n + 1`
-   import
    -   `import ...`
    -   `from ... import ...`

The followings (and the other features) are unavailable:

-   complicated assignment statement
    -   `x, *xs = a`
    -   `a[l : r] = b`
-   expression statement
    -   `print("Hello")`
    -   There are no features which have side-effects, so expression statements are meaningless.
-   yield
-   raise
-   break
-   continue


### Compound statement

The followings are available:

-   if-else
    -   `if cond: ... else: ...`
    -   `if cond: ... elif cond: ... else: ...`
-   for
    -   `for x in xs: ...`
-   def
    -   `def f(x): ...`
    -   `def f(x: t) -> t: ...`

The followings (and the other features) are unavailable:

-   if
    -   `if: ...`
-   while
-   try
-   with
-   class


## Syntax

### import

`import` statements have no effects. They are just ignored.

### def

`def` defines a function. This is available only the toplevel.
The self-recursion is allowed, but mutual-recursion is not allowed.

The followings cause undefined behaviors:

-   name conflicts between global variables and the function itself
-   name conflicts between global variables and local variables

### return

`return` declares the result value of `def`.
All code paths of `def` must contain at least one `return`.

### assignment

The assignments `x = a` binds the value `a` to the variable `x`.
You can use this on toplevel and in `def`.

You can think this as `let x = a in ...` of ML.

### if-else

`if-else` branches the execution.
This is a syntax suger of the combinational operator `... if ... else ...`.

It must have the `else:` branch, and all branches must end with `return`.

TODO: allow more flexible `if` statements?

### for

`for` runs loops.
This is a syntax suger of some list operations.

There must be declarations of target variables.
A target variable is a usual variable or a variable which is assigned a list using the special keyword `None`.
The `for` has the assignments to the target variables.
When a list target variable is used, the container which `for` runs must be `range(...)` or `enumerate(...)`, and the size of the list target and the size of container must relates.

For example, the below is a valid form of `for`-loop. This is a syntax suger of `a = [f(i) for i in range(N)]; b = sum(sum(1 for j in range(M)) for i in range(N))`.

``` python
    a = [None for _ in range(N)]
    b = 0
    for i in range(N):
        a[i] = f(i)
        for j in range(M):
            b += 1
```

The below is also a valid form of `for`-loop.

``` python
    a = [None for _ in range(N + 1)]
    a[0] = 1
    for i in range(N):
        a[i + 1] = a[i] + g(i)
```

The below is not a valid form of `for`-loop.

``` python
    b = [3, 5, 8]
    a = [False for _ in range(10)]
    for b_i in b:
        a[b_i] = True
```


## Semantics

### Python compatibility

This language is compatible with Python.
This means that, if the execution successfully stops without any undefined behavior on both interpreter of this language and Python, then the result is the same.


### types

Each terms uniquely belongs its type (Church-style).
Users can make mistakes about these types. Implementations are responsible for checking these types.

-   `int` type represents the set of integers.
-   `bool` type represents the set of truth values. It has two values `True` and `False`.
-   For any type `T`, `Sequence[T]` type represents the set of finite or infinite sequences of values in `T`. They are immutable.
-   For any types `T1`, `T2`, ..., and `Tn`, `Tuple[T1, T2, ..., Tn]` is the direct product of `T1`, `T2`, ..., and `Tn`.
    -   `n` is a non-negative number.
-   For any types `T1`, `T2`, ..., `Tn`, and `R`, `Callable[[T1, T2, ..., Tn], R]` is the set of functions from `T1`, `T2`, ..., `Tn` to `R`.
    -   `n` is a non-negative number.
    -   This is used only on documents.


### annotational types

Some terms may have some annotational types (Curry-style).
Users are responsible to write correctly about these types, or undefined behavior appears. Implementations can use these types as hints.

-   For any type `T`, `Optional[T]` is an alias of `T`.
-   For any type `T`, `Iterator[T]` is an alias of `Sequence[T]`.
-   For any type `T`, `List[T]` is an subtype of `Sequence[T]`. This is the set of all finite sequences.

There are the additional rule for `Iterator[T]`. When a variable is annotated as `Iterator[T]`, you cannot use the object twice. For example, `list(xs) == list(xs)` for a variable `xs: Iterator[int]` causes an undefined behavior.
This means that, implementations of Jikka can entirely ignores the differences of lists and iterators of the standard Python, but we don't think this fact as the incompatibility with Python.


## Standard Library

### builtin operators from Python

arithmetical functions (`Callable[[int], int]`, `Callable[[int, int], int]`):

-   `-` negation
-   `+` addition
-   `-` subtraction
-   `*` multiplication
-   `//` division (floor, consistent to `%`)
    -   It means `(x // y) * y + (x % y) == x` always holds.
-   `%` modulo (always positive)
-   `**` power

logical functions (`Callable[[bool], bool]`, `Callable[[bool, bool], bool]`):

-   `not` not
-   `and` and
    -   No short circuit exists.
-   `or` or
    -   No short circuit exists.

bitwise functions (`Callable[[int], int]`, `Callable[[int, int], int]`):

-   `~` bitwise-not
-   `&` bitwise-and
-   `|` bitwise-or
-   `^` bitwise-xor
-   `<<` left shift
-   `>>` right shift

arithmetical relations (`Callable[[int, int], bool]`):

-   `<` less-than
    -   Please note that combinational notations like `a < b < c` are not supported.
-   `>` greater-than
-   `<=` less-or-equal
-   `>=` greater-or-equal

the combinational operator (polymorphic, `Callable[[T, bool, T], T]`):

-   `... if ... else ...`
    -   Short circuits exist.

equality relations (polymorphic, `Callable[[T, T], bool]` only for `int` and `bool`).

-   `==` equal
    -   Please note that combinational notations like `a == b == c` are not supported.
-   `!=` not-equal


### additional builtin operators

WARNING: these ops breaks compatibility with Python. `from jikka.compat import *` disables them.

arithmetical functions (`Callable[[int, int], int]`):

-   `/^` division (ceil)
    -   same to `(x + y - 1) // y`
    -   TODO: is this definition appropriate for negative numbers?
-   `%^` modulo (ceil, consistent to `/^`)
    -   It means `(x /^ y) * y + (x %^ y) == x` always holds.
-   `<?` min
    -   comes from [old GCC](https://gcc.gnu.org/onlinedocs/gcc-3.3.2/gcc/Min-and-Max.html)
    -   The operators `<?` and `>?` have the precedence between bit-ops and comparisons.
        They are left-to-right associative.
        For example, `a ^ 1 <? b == c` is `((a ^ 1) <? b) == c`.
        `a <? b <? c ?> d <? e` is `((((a <? b) <? c) ?> d) <? e)`.
-   `>?` max
    -   same to min

logical functions (`Callable[[bool, bool], bool]`):

-   `implies` implication
    -   No short circuit exists.
    -   The operator `implies` has the precedence between `or`-op and `if`-`else`.
        They are right-to-left associative.
        For example, `a implies b if c else d implies e` is `(a implies b) if (c) eles (d implies e)`
        `a implies b implies c` is `a implies (b implies c)`.

The operators `<?` and `>?` have the precedence between bit ops and comparisons.
They are left-to-right associative.
For example, `a ^ 1 <? b == c` is `((a ^ 1) <? b) == c`.
`a <? b <? c ?> d <? e` is `((((a <? b) <? c) ?> d) <? e)`.

### builtin small functions

From the default Python:

-   `abs(x: int) -> nat`
-   `min(x: int, y: int) -> int`
-   `max(x: int, y: int) -> int`
-   `pow(x: int, y: int) -> int`
-   `pow(x: int, y: int, mod: nat) -> nat`
    -   WARNING: negative exp is allowed from Python 3.8
    -   `mod` must be positive
-   `len(xs: List[T]) -> nat`

Not from the default Python:

-   `gcd(x: int, y: int) -> int`
    -   If you want, you can write `from math import *` before using this.
-   `lcm(x: int, y: int) -> int`
    -   If you want, you can write `from math import *` before using this.
    -   WARNING: this is defined from Python 3.9
-   `floordiv(x: int, y: int) -> int`
    -   same to `x // y`
-   `ceildiv(x: int, y: int) -> int`
    -   same to `(x + y - 1) // y`
    -   TODO: is this definition appropriate for negative numbers?
-   `fact(x: nat) -> nat`
-   `choose(n: nat, r: nat): nat`
    -   `n >= r` must be holds
-   `permute(n: nat, r: nat) -> nat`
    -   `n >= r` must be holds
-   `multichoose(n: nat, r: nat) -> nat`
    -   `n >= r` must be holds
-   `inv(x: int, mod: nat) -> nat`
    -   `x` must not be a multiple of `mod`
    -   `mod` must be positive


### builtin big functions

From the default Python:

-   `sum(xs: Iterator[int]) -> int`
-   `min(xs: List[int]) -> int`
    -   `xs` must be non empty
-   `max(xs: List[int]) -> int`
    -   `xs` must be non empty
-   `all(xs: Iterator[bool]) -> bool`
-   `any(xs: Iterator[bool]) -> bool`
-   `sorted(xs: Iterator[int]) -> List[int]`
-   `list(xs: Iterator[int]) -> List[int]`
-   `reversed(xs: Iterator[int]) -> Iterator[int]`
-   `range(stop: int) -> Iterator[int]`
-   `range(start: int, stop: int) -> Iterator[int]`
-   `range(start: int, stop: int, step: int) -> Iterator[int]`

Not from the default Python:

-   `product(xs: Iterator[int]) -> int`
-   `argmin(xs: List[int]) -> nat`
    -   `xs` must be non empty
-   `argmax(xs: List[int]) -> nat`
    -   `xs` must be non empty
