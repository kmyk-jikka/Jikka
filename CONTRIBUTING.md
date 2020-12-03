# Contributing

## TODO

-   write the core optimizer
-   use a consistent error system
-   use a robust gensym system
-   put more precise location info to more nodes of AST
-   improve our Python subset
-   write the code generater of C++
-   write the parser of our ML-like language
-   prepare a system to define rewrite rules easily

## Tools

### Tests

Use the following command to run tests. Now [Hspec](https://hspec.github.io/) and [Doctest](https://hackage.haskell.org/package/doctest) are enabled.

``` console
$ stack test
```

The GitHub Actions for tests are defined at [.github/workflows/test.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/test.yml).

### Formatting

Use the following commands to run format-checking. Now [Ormolu](https://github.com/tweag/ormolu) and [HLint](https://github.com/ndmitchell/hlint) are enabled.

``` console
$ stack exec ormolu -- --mode=check $(find src app test -name \*.hs)
$ stack exec hlint -- src app test
```

The GitHub Actions for formatting are defined at [.github/workflows/format.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/format.yml).

### Documents

[Haddock](https://www.haskell.org/haddock/) is used for internal documents of implementation.
Run the following command to generate documents.

``` console
$ stack haddock
```
