# CONTRIBUTING.md

(このドキュメントの日本語バージョン: [CONTRIBUTING.ja.md](https://github.com/kmyk/Jikka/blob/master/CONTRIBUTING.ja.md))

## Development process and conventions

### Execution

Please use [Stack](https://www.haskellstack.org/) for development.
If you are using Ubuntu, you can install Stack with `$ sudo apt install haskell-stack`.

To run Jikka after you modified the source code, use the following commands:

- `$ stack run convert XXX.py` runs Jikka which is locally built with `src/`
- `$ stack run -- convert --target core XXX.py` prints our core language instead of C++
- `$ stack run -- convert --target rpython XXX.py` prints our restricted Python instead of C++
- `$ stack run execute XXX.py < YYY.in` executes AST of our core language directly
- `$ stack run -- execute -- --target rpython XXX.py < YYY.in` executes AST of our restricted Python directly
- `$ python3 XXX.py < YYY.in'` or `$ python3 -c 'import XXX; print(XXX.main(1, 2, [3, 4, 5]))'` runs as the standard Python

### Tests

Use the following commands to run tests.
[Hspec](https://hspec.github.io/) and [Doctest](https://hackage.haskell.org/package/doctest) are enabled.
Also contents of [examples/](https://github.com/kmyk/Jikka/tree/master/examples) directory are verified.

```console
$ stack test
$ python3 examples/integration_tests.py
```

The GitHub Actions for tests is defined at [.github/workflows/test.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/test.yml).

#### Integration Tests

`$ python3 scripts/integration_tests.py` runs tests using files under [examples/](https://github.com/kmyk/Jikka/tree/master/examples).
For each Python file `examples/XXX.py`, it finds test cases like `examples/data/XXX.YYY.in` `examples/data/XXX.YYY.out` or generates test cases from `examples/data/XXX.ZZZ.generator.py` `examples/data/XXX.solver.py`, and then executes it as Python / our restricted Python / our core / C++, and verifies the results.

With `$ python3 scripts/integration_tests.py -k XXX`, you can run only specific tests which contains `XXX` in its file name.

We are collecting more test cases.
When you solved a real problems of competitive programmings using Jikka, please send a pull request which adds it as a new test case.

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
