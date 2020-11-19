# Language Specification

-   TODO: Fix the syntax of types. `def solve(n: int, a: Array[int, n]) -> int:` is invalid in Python. We should use `Array[int, "n"]` instead.
-   TODO: Fix the syntax of exprs around operators.

## Overview

a very restricted subset of Python + additional builtin functions


## Syntax

### Lexical Analysis

This is the almost same to Python.

### Grammer

```ebnf
<program> ::= <toplevel-decl> +

# types
<type> ::= "int"
         | "nat"
         | "bool"
         | "Interval" "[" <expr>, <expr> "]"
         | "List" "[" <type> "]"
         | "Array" "[" <type> "," <expr> "]"

# literals
<int-literal> ::= ...
<bool-literal> ::= "True" | "False"
<literal> ::= <int-literal>
            | <bool-literal>

# names
<var-name> ::= ...
<fun-name> ::= ...
<var-name-or-underscore> ::= <var-name>
                           | "_"

# args
<actual-args> ::= EMPTY
                | <expr> ("," <expr>) *
<formal-args> ::= EMPTY
                | <var-name> ":" <type> ("," <var-name> ":" <type>) *

# lists
<comprehension> ::= <expr> "for" <var-name-or-underscore> "in" <expr> [ "if" <expr> ]
<list-shape> ::= '[' "None" "for" "_" "in" "range" "(" <expr> ")" "]"
               | '[' <list-shape> "for" "_" "in" "range" "(" <expr> ")" "]"
<list-subscript> ::= '[' Expr ']'
                   | '[' Expr ']' <list-subscript>

# exprs
<atom> ::= <var-name>
         | <literal>
         | "(" <expr> ")"
<expr> ::= <atom>
         | <unary-op> <atom>
         | <atom> <binary-op> <atom>
         | <atom> "[" <expr> "]"
         | "[" <actual-args> "]"
         | "[" <comprehension> "]"
         | <fun-name> "(" <actual-args> ")"
         | <fun-name> "(" <comprehension> ")"
         | <expr> "if" <expr> "else" <expr>
<unary-op> ::= ...
<binary-op> ::= ...

# simple statements
<simple-stmt> ::= <var-name> ":" <type> "=" <expr>
                | <var-name> ":" <type> "=" <list-shape>
                | <var-name> <list-subscript> "=" <expr>
                | "assert" <expr>
                | "return" <expr>

# compound statements
<compound-stmt> ::= <if-stmt>
                  | <for-stmt>
<if-stmt> ::= "if" <expr> ":" <suite>
              ("elif" <expr> ":" <suite>) *
              "else" <expr> ":" <suite>
<for-stmt> ::= "for" <var-name> "in" <expr> ":" <suite>

# statements
<suite> ::= NEWLINE INDENT NEWLINE + (<statement> NEWLINE +) + DEDENT
<statement> ::= <simple-statement>
              | <compound-statement>

# toplevel declarations
<from-import> ::= "from" "jikka" "." "compat" "import" "*"
                | "from" "math" "import" "*"
<function-decl> ::= "def" <fun-name> "(" <formal-args> ")" "->" <type> ":" <suite>
<toplevel-decl> ::= <from-import> NEWLINE
                  | <assignment-stmt> NEWLINE
                  | <function-decl>
```


## Semantics

### Python compatibility

This language is compatible with Python.
This means that, if the execution successfully stops on both interpreter of this language and Python, the result is the same.

### intrinsic types

Each terms uniquely belongs its Church-style type.

-   `int` type represents the set of integers.
-   `bool` type represents the set of truth values. It has two values `True` and `False`.
-   For any type `T`, `List[T]` type represents the set of finite sequences of values in `T`. They are immutable.

Also, some additional types exist.

-   For any type `T`, `Iterator[T]` type represents the class of iterator objects.
    -   This type is used internally only for the compatibility with Python.
    -   You cannot bind values of `Iterator[T]` to any variables.
    -   For any types `T` and `S`, if `S` is a subtype of `T`, then `List[S]` is treated as a subtype of `Iterator[T]` (covariant).
-   For any types `T1`, `T2`, ..., `Tn`, and `R`, `Callable[[T1, T2, ..., Tn], R]` is the set of functions from `T1`, `T2`, ..., `Tn` to `R`.
    -   This is used only on documents.

They all come from the standard types of Python.

### annotational types

Some terms have some Curry-style types.

-   `nat` type represents the set of natural numbers.
    -   `nat` is not a standard type of Python.
    -   `nat` is a subtype of `int`.
    -   `0` is a natural number.
-   For integers `l` and `r` which `l <= r` holds, `Interval[l, r]` type represents the closed interval `[l, r]`.
    -   `Interval[l, r]` is a subtype of `int`.
    -   `Interval[l, r]` is a subtype of `nat` when `l >= 0`.
    -   `r` is included.
-   For any type `T` and an integer `n`, `Array[T, n]` type represents the set of sequences of values in `T` with the length `n`. They are immutable.
    -   `Array[T, n]` is not a standard type of Python.
    -   `Array[T, n]` is a subtype of `List[T]`.

### single assignment

The keyword `None` is just a placeholder.
It's not a value.

The behavior is undefined when a variable without value (i.e. in `None`) is used as an argument of a function call or an operation. However, if possible, the compiler should detect it and stop as a compiler error.

### iterators

For any type `T`, `Iterator[T]` is the special type represents the set of iterator objects of values in `T`. This is not immutable, so used only as a types of builtin functions. You cannot make a variable which has the iterator type.

TODO: should we allow iterators which have the infinite length?

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
-   `or` or

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

the combinational function (polymorphic, `Callable[[T, bool, T], T]`):

-   `... if ... else ...`

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
-   `<?` min
    -   comes from [old GCC](https://gcc.gnu.org/onlinedocs/gcc-3.3.2/gcc/Min-and-Max.html)
-   `>?` max
    -   comes from [old GCC](https://gcc.gnu.org/onlinedocs/gcc-3.3.2/gcc/Min-and-Max.html)

logical functions (`Callable[[bool, bool], bool]`):

-   `implies` implication


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
