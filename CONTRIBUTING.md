# CONTRIBUTING.md

(このドキュメントの日本語バージョン: [CONTRIBUTING.ja.md](https://github.com/kmyk/Jikka/blob/master/CONTRIBUTING.ja.md))

## How can I contribute to this project?

Currently, you can help us with the following ways:

- Please find tasks of competitive programming which are seems to be automatically solved, and reporting them in [comments of an issue](https://github.com/kmyk/Jikka/issues/25).
  - If possible, please send to us pull requests which add Python codes with stupid algorithms and test cases for the tasks to [examples/wip/](https://github.com/kmyk/Jikka/tree/master/examples/wip) directory.
  - The problems which you found and Python codes which you sent are used for testing.

## Development process and conventions

### Tests

Use the following commands to run tests.
[Hspec](https://hspec.github.io/) and [Doctest](https://hackage.haskell.org/package/doctest) are enabled.
Also contents of [examples/](https://github.com/kmyk/Jikka/tree/master/examples) directory are verified.

```console
$ stack test
$ bash examples/test.sh
```

The GitHub Actions for tests is defined at [.github/workflows/test.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/test.yml).

### Formatting

There is a script at `scripte/pre-commit` which checkes formatting of all files.
We recommend to configure this as the pre-commit hook with running `$ ln -s $(pwd)/scripts/pre-commit .git/hooks/pre-commit`.

#### Haskell

[Ormolu](https://github.com/tweag/ormolu) and [HLint](https://github.com/ndmitchell/hlint) are enabled.
Use the following commands to check formatting.

```console
$ stack exec ormolu -- --mode=check $(find src app test -name \*.hs)
$ stack exec hlint -- src app test
```

Use the following command to fix formatting automatically as possible.

```console
$ stack exec ormolu -- --mode=check $(find src app test -name \*.hs)
```

The GitHub Actions for formatting if defined at [.github/workflows/format.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/format.yml).

#### C++

[clang-format](https://clang.llvm.org/docs/ClangFormat.html) is enabled.

#### Python

[yapf](https://github.com/google/yapf) and [isort](https://github.com/PyCQA/isort) are enabled.
You can install these with `$ pip3 install -r scripts/requirements.txt`.

#### Markdown

[Prettier](https://prettier.io/) is enabled.
you can install this with installing [Yarn](https://yarnpkg.com/) and running `$ yarn install`.

#### YAML

[Prettier](https://prettier.io/) is enabled.
you can install this with installing [Yarn](https://yarnpkg.com/) and running `$ yarn install`.

### Documents

[Haddock](https://www.haskell.org/haddock/) is used for internal documents of internal implementation.
Run the following command to generate documents locally.

```console
$ stack haddock
```

### Commit messages

Use [Conventional Commits](https://www.conventionalcommits.org/).

### Versioning

Conforming to [Haskell Package Versioning Policy](https://pvp.haskell.org/).
