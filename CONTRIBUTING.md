# CONTRIBUTING.md

(このドキュメントの日本語バージョン: [CONTRIBUTING.ja.md](https://github.com/kmyk/Jikka/blob/master/CONTRIBUTING.ja.md))

## Development process and conventions

### Tests

Use the following command to run tests.
Now [Hspec](https://hspec.github.io/) and [Doctest](https://hackage.haskell.org/package/doctest) are enabled.
Also contents of [examples/](https://github.com/kmyk/Jikka/tree/master/examples) directory are verified.

``` console
$ stack test
$ bash examples/test.sh
```

The GitHub Actions for tests are defined at [.github/workflows/test.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/test.yml).

### Formatting

Use the following commands to run format-checking. Now [Ormolu](https://github.com/tweag/ormolu) and [HLint](https://github.com/ndmitchell/hlint) are enabled.

``` console
$ stack exec ormolu -- --mode=check $(find src app test -name \*.hs)
$ stack exec hlint -- src app test
```

Use the following command to fix format automatically as possible.

``` console
$ stack exec ormolu -- --mode=check $(find src app test -name \*.hs)
```

The GitHub Actions for formatting are defined at [.github/workflows/format.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/format.yml).

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
