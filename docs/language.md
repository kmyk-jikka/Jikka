# Language Specification

(このドキュメントの日本語バージョン: [language.ja.md](https://github.com/kmyk/Jikka/blob/master/docs/language.ja.md))

## TL;DR

-   It has syntax the same as Python.  It is basically a subset language of Python.
-   It has semantics similar to OCaml and Haskell.  It's statically typed and has type inference, and all values are immutable.
-   It has some builtin functions and operators for competitive programming.  Implementations should optimize programs to reduce computational complexity.

## About BREAKING CHANGE

This language specification is in initial development.
While the second major version is zero (`v5.0.y.z`), anything MAY change at any time.
See also [item (4.) of Semantic Versioning](https://semver.org/#spec-item-4).


## Syntax

### Lexical Analysis

It's almost the same as Python.

### Grammer

It's basically the same as Python.

There are many restrictions, which usually don't affect usage for competitive programming.
These restrictions simplify the language and make it easy to optimize.

### Expression

The followings are available:

-   literals
    -   `0`, `1`, `2`, ...
    -   `True`, `False`
-   lists
    -   `[a, b, c, ...]`
    -   `[... for ... in ...]`
-   generators
    -   `(... for ... in ...)`
-   subscriptions
    -   `a[i]`
-   function call
    -   `f(a, b, c, ...)`
    -   `f(... for ... in ...)`
-   operators
    -   See the "Standard Library" section.
-   conditional operator
    -   `... if ... eles ...`
-   comprehensions with filtering
    -   `... for ... in ... if ...`
-   slicing
    -   `a[l : r]`
-   lambdas
    -   `lambda x: ...`

The followings are unavailable:

-   list with stars
    -   `[x, *xs]`
-   walrus operator
    -   `x := a`
-   attribute reference
    -   `foo.bar`
-   complicated funcion calls
    -   `f(foo=a, bar=b)`
    -   `f(*args)`


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

The followings are unavailable:

-   complicated assignment statement
    -   `x, *xs = a`
    -   `a[l : r] = b`
-   expression statement
    -   `print("Hello")`
-   yield
-   raise
-   break
-   continue


### Compound statement

The followings are available:

-   if
    -   `if: ...`
    -   `if cond: ... else: ...`
    -   `if cond: ... elif cond: ... else: ...`
-   for
    -   `for x in xs: ...`
-   def
    -   `def f(x): ...`
    -   `def f(x: t) -> t: ...`

The followings are unavailable:

-   while
-   try
-   with
-   class


## Semantics (Statements)

### import

`import` statement has no effects.
It's just ignored.

### def

`def` statement defines a function.

-   You can use this only on the toplevel.
-   You can use recursion.
-   You cannot overwrite functions that are already defined.
-   In its body, you cannot use identifiers that will be defined in the future. This restriction means you cannot use mutul-recursion.

### return

`return` statement declares the result value of the function, which is defined with `def` statement.
All code paths of a `def` statement must contain at least one `return` statement.

### assignment

The assignment `x = a` statement binds the value `a` to the variable `x`.

-   You can use this on toplevel and in `def` statements.
-   If it's not on the toplevel, you can overwrite variables that are already defined, and change the types of variables.
-   This statement is similar to `let x = a in ...` of ML.
-   If its lvalue has subscriptions, this is treated as an augmented assignment.

### augmented assignment

The augmented assignment `x @= a` statement binds the value `x @ a` to the variable `x`.

-   You can use this only in `def` statements.

### if

`if` statement branches execution.

-   After `if` statement, you cannot use variables that are defined only one of either `if` clause or `else` clause.

### for

`for` statement repeats execution.

-   After `for` statement, you cannot use variables that are newly defined in the `for` statement.
-   `else` clause is not available.
-   You cannot use `return` statements in `for` statements.

### assert

`assert` statement introduces undefined behaviors.

-   Implementations can use `assert` statements as hints for optimization.


## Semantics (Exprs)

It's basically the same as Python.


## Semantics (Types)

The types are inductively defined with the following:

-   `int` is a type. This type represents the set of integers.
-   `bool` is a type. This type represents the set of truth values.
-   For any type `T`, `List[T]` is a type. This type represents the set of finite sequences of values in `T`.
-   For a non-negative integer `n` and any types `T1`, `T2`, ... and `Tn`, `Tuple[T1, T2, ..., Tn]` is a type. This type represents the direct product of `T1`, `T2`, ..., and `Tn`.
-   For a non-negative integer `n` and any types `T1`, `T2`, ..., `Tn` and `R`, `Callable[[T1, T2, ..., Tn], R]` is a type. This type represents the set of `n`-ary functions from `T1`, `T2`, ..., `Tn` to `R`.
-   Only things which are written above are types.

Each expression or each value uniquely belongs to its type, i.e., it's Church-style.

-   You must take care of the fact that the values of `List[T]` are immutable.


## Entry points

### `solve` function

`solve` function is the entry point of a program.

-   You must define the function named as `solve` in all programs.
-   The types of arguments of `solve` function must be one of `int`, `List[int]`, `List[List[int]]`, `List[List[List[int]]]`, ....
-   The types of return values of `solve` function must be one of `int`, `List[int]`, `List[List[int]]`, `List[List[List[int]]]`, ..., or `Tuple` of these types.

### `main` function

`main` function is the special function to specify input/output format.

-   You can write a program without `main` function.
-   `main` function must have no arguments.
-   The type of the return value of `main` function must be `None`.
-   You can use only the following statements in `main` function.
    -   `x = int(input())`
    -   `x, y, z = map(int, input().split())`
    -   `xs = list(map(int, input().split())); assert len(xs) == n`
    -   `xs = list(range(n)); for i in range(n): xs[i] = ...`
    -   `x, y, z = solve(a, b, c)`
    -   `print(x, y, z)`
    -   `for i in range(n): ...`

### Toplevel expression statements

You can call `main` function in the toplevel of a program.

It must be either of the following statements:

-   `main()`
-   `if __name__ == "__main__": main()`


## Standard Library

### builtin operators from Python

arithmetical operators (`Callable[[int], int]`, `Callable[[int, int], int]`):

-   `-` negation
-   `+` addition
-   `-` subtraction
-   `*` multiplication
-   `//` division (floor, consistent to `%`)
    -   It means `(x // y) * y + (x % y) == x` always holds.
-   `%` modulo (always positive)
-   `**` power

logical operators (`Callable[[bool], bool]`, `Callable[[bool, bool], bool]`):

-   `not` not
-   `and` and
    -   No short circuit exists.
-   `or` or
    -   No short circuit exists.

bitwise operators (`Callable[[int], int]`, `Callable[[int, int], int]`):

-   `~` bitwise-not
-   `&` bitwise-and
-   `|` bitwise-or
-   `^` bitwise-xor
-   `<<` left shift
-   `>>` right shift

comparators (`Callable[[T, T], bool]`):

-   `==` equal
-   `!=` not-equal
-   `<` less-than
-   `>` greater-than
-   `<=` less-or-equal
-   `>=` greater-or-equal

the combinational operator (`Callable[[T, bool, T], T]`):

-   `... if ... else ...`
    -   No short circuit exists.


### additional builtin operators

arithmetical operators (`Callable[[int, int], int]`):

-   `/^` division (ceil)
    -   This is same to `(x + y - 1) // y`.
-   `%^` modulo (ceil, consistent to `/^`)
    -   It means `(x /^ y) * y + (x %^ y) == x` always holds.
-   `<?` min
    -   This comes from [old GCC](https://gcc.gnu.org/onlinedocs/gcc-3.3.2/gcc/Min-and-Max.html).
    -   The operators `<?` and `>?` have the precedence between bit-ops and comparisons.
        They are left-to-right associative.
        For example, `a ^ 1 <? b == c` is `((a ^ 1) <? b) == c`, and `a <? b <? c ?> d <? e` is `((((a <? b) <? c) ?> d) <? e)`.
-   `>?` max
    -   same to min

logical operators (`Callable[[bool, bool], bool]`):

-   `implies` implication
    -   No short circuit exists.
    -   The operator `implies` has the precedence between `or`-op and `if`-`else`.
        They are right-to-left associative.
        For example, `a implies b if c else d implies e` is `(a implies b) if (c) eles (d implies e)`, and `a implies b implies c` is `a implies (b implies c)`.

### builtin functions from Python

integer functions:

-   `abs(x: int) -> int`
-   `min(x: T, y: T) -> T`
-   `max(x: T, y: T) -> T`
-   `pow(x: int, y: int) -> int`
-   `pow(x: int, y: int, mod: int) -> int`

list functions:

-   `len(xs: List[T]) -> int`
-   `sum(xs: List[int]) -> int`
-   `min(xs: List[T]) -> T`
-   `max(xs: List[T]) -> T`
-   `all(xs: List[bool]) -> bool`
-   `any(xs: List[bool]) -> bool`
-   `sorted(xs: List[T]) -> List[T]`
-   `list(xs: List[T]) -> List[T]`
-   `reversed(xs: List[T]) -> List[T]`
-   `range(stop: int) -> List[int]`
-   `range(start: int, stop: int) -> List[int]`
-   `range(start: int, stop: int, step: int) -> List[int]`

### additional builtin functions

basic integer functions:

-   `gcd(x: int, y: int) -> int`
-   `lcm(x: int, y: int) -> int`
-   `floordiv(x: int, y: int) -> int`
    -   This is same to `x // y`.
-   `floormod(x: int, y: int) -> int`
    -   This is same to `x % y`.
-   `ceildiv(x: int, y: int) -> int`
    -   This is same to `(x + y - 1) // y`.
-   `ceilmod(x: int, y: int) -> int`

list functions:

-   `product(xs: Iterator[int]) -> int`
-   `argmin(xs: List[T]) -> int`
    -   `xs` must be non empty.
-   `argmax(xs: List[T]) -> int`
    -   `xs` must be non empty.

combinatorics functions:

-   `fact(x: int) -> int`
-   `choose(n: int, r: int): int`
    -   `n >= r` must be holds.
-   `permute(n: int, r: int) -> int`
    -   `n >= r` must be holds.
-   `multichoose(n: int, r: int) -> int`
    -   `n >= r` must be holds.

modular functions:

-   `modinv(x: int, mod: int) -> int`
    -   `x` must not be a multiple of `mod`.
    -   `mod` must be positive.

TODO:

-   matrix functions:
-   matrix functions on modular arithmetic:
