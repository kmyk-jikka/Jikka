# CONTRIBUTING.ja.md

(The English version of this document: [CONTRIBUTING.md](https://github.com/kmyk/Jikka/blob/master/CONTRIBUTING.md))

## Development process and conventions

### Tests

テストには以下のコマンドを実行してください。
[Hspec](https://hspec.github.io/) と [Doctest](https://hackage.haskell.org/package/doctest) が有効になっています。
また [examples/](https://github.com/kmyk/Jikka/tree/master/examples) ディレクトリの中身が検査されます。

```console
$ stack test
$ python3 scripts/integration_tests.py
```

テストのための GitHub Actions は [.github/workflows/test.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/test.yml) で定義されています。

#### Integration Tests

`$ python3 scripts/integration_tests.py` を実行すると [examples/](https://github.com/kmyk/Jikka/tree/master/examples) ディレクトリの中身を用いてテストが実行されます。
`examples/XXX.py` という Python ファイルごとに、`examples/data/XXX.YYY.in` `examples/data/XXX.YYY.out` のようなファイル名のテストケースを探しまた `examples/data/XXX.ZZZ.generator.py` `examples/data/XXX.solver.py` などからテストケースを生成して、これを Python / 制限された Python / core / C++ として実行してそれぞれすべてが正しく動作することを検証します。

`$ python3 scripts/integration_tests.py -k XXX` とすると `XXX` をファイル名に含むテストだけを実行することができます。

テストケースが多くて困ることはあまりありません。
Jikka を用いて実際のコンテストの問題が解けたとき (あるいは解けそうなとき) は、ぜひそれをテストケースとして追加するプルリクエストを送ってください。

### Formatting

すべてのフォーマットをまとめて検査してくれるスクリプトが `scripte/pre-commit` にあります。
`$ ln -s $(pwd)/scripts/pre-commit .git/hooks/pre-commit` を実行して pre-commit hook に設定しておくとよいでしょう。

#### Haskell

[Ormolu](https://github.com/tweag/ormolu) と [HLint](https://github.com/ndmitchell/hlint) が有効になっています。
フォーマットの検査には以下のコマンドを実行してください。

```console
$ stack exec ormolu -- --mode=check $(find src app test -name \*.hs)
$ stack exec hlint -- src app test
```

フォーマットを可能な範囲で自動で修正するには以下のコマンドを実行してください。

```console
$ stack exec ormolu -- --mode=inplace $(find src app test -name \*.hs)
```

フォーマットのための GitHub Actions は [.github/workflows/format.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/format.yml) で定義されています。

#### C++

[clang-format](https://clang.llvm.org/docs/ClangFormat.html) が有効になっています。

#### Python

[yapf](https://github.com/google/yapf) と [isort](https://github.com/PyCQA/isort) が有効になっています。
これらは `$ pip3 install -r scripts/requirements.txt` とすればインストールできます。

#### Markdown

[Prettier](https://prettier.io/) が有効になっています。
これは [Yarn](https://yarnpkg.com/) を使って `$ yarn install` とすればインストールできます。

#### YAML

[Prettier](https://prettier.io/) が有効になっています。
これは [Yarn](https://yarnpkg.com/) を使って `$ yarn install` とすればインストールできます。

### Documents

実装内部のドキュメントには [Haddock](https://www.haskell.org/haddock/) が使われています。
ローカルでドキュメントを生成するには以下のコマンドを実行してください。

```console
$ stack haddock
```

### Commit messages

[Conventional Commits](https://www.conventionalcommits.org/ja/v1.0.0/) を使ってください。

### Versioning

[Haskell Package Versioning Policy](https://pvp.haskell.org/) に準拠しています。
