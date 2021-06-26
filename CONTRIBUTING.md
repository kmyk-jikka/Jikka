# CONTRIBUTING.md

(このドキュメントの日本語バージョン: [CONTRIBUTING.ja.md](https://github.com/kmyk/Jikka/blob/master/CONTRIBUTING.ja.md))


## How can I contribute to this project?

Currently, you can help us with the following ways:

-   Finding tasks of competitive programming which are seems to be automatically solved, and sending to us Python codes with stupid algorithms for the tasks.
    -   The Python codes which you sent are used for testing.


## Development process and conventions

### Tests

Use the following commands to run tests.
[Hspec](https://hspec.github.io/) and [Doctest](https://hackage.haskell.org/package/doctest) are enabled.
Also contents of [examples/](https://github.com/kmyk/Jikka/tree/master/examples) directory are verified.

``` console
$ stack test
$ bash examples/test.sh
```

The GitHub Actions for tests is defined at [.github/workflows/test.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/test.yml).

### Formatting

Use the following commands to check formatting.
[Ormolu](https://github.com/tweag/ormolu) and [HLint](https://github.com/ndmitchell/hlint) are enabled.

``` console
$ stack exec ormolu -- --mode=check $(find src app test -name \*.hs)
$ stack exec hlint -- src app test
```

Use the following command to fix formatting automatically as possible.

``` console
$ stack exec ormolu -- --mode=check $(find src app test -name \*.hs)
```

The GitHub Actions for formatting if defined at [.github/workflows/format.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/format.yml).

### Documents

[Haddock](https://www.haskell.org/haddock/) is used for internal documents of internal implementation.
Run the following command to generate documents locally.

``` console
$ stack haddock
```

### Commit messages

Use [Conventional Commits](https://www.conventionalcommits.org/).

### Versioning

[Semantic Versioning](https://semver.org/lang/ja/) is used with regard to the Python-like language as its public API.
However, it has two MAJOR version that come from [Haskell Package Versioning Policy](https://pvp.haskell.org/).
