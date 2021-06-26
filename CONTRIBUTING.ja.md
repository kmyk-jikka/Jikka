# CONTRIBUTING.ja.md

(The English version of this document: [CONTRIBUTING.md](https://github.com/kmyk/Jikka/blob/master/CONTRIBUTING.md))

## Tools

### Tests

テストには以下のコマンドを実行してください。
現在は [Hspec](https://hspec.github.io/) と [Doctest](https://hackage.haskell.org/package/doctest) が有効になっています。
また [examples/](https://github.com/kmyk/Jikka/tree/master/examples) ディレクトリの中身が検査されます。

``` console
$ stack test
$ bash examples/test.sh
```

テストのための GitHub Actions は [.github/workflows/test.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/test.yml) で定義されています。

### Formatting

フォーマットの検査には以下のコマンドを実行してください。
現在は [Ormolu](https://github.com/tweag/ormolu) と [HLint](https://github.com/ndmitchell/hlint) が有効になっています。

``` console
$ stack exec ormolu -- --mode=check $(find src app test -name \*.hs)
$ stack exec hlint -- src app test
```

フォーマットを可能な範囲で自動で修正するには以下のコマンドを実行してください。

``` console
$ stack exec ormolu -- --mode=inplace $(find src app test -name \*.hs)
```

フォーマットのための GitHub Actions は [.github/workflows/format.yml](https://github.com/kmyk/Jikka/blob/master/.github/workflows/format.yml) で定義されています。

### Documents

実装内部のドキュメントには [Haddock](https://www.haskell.org/haddock/) が使われています。
ローカルでドキュメントを生成するには以下のコマンドを実行してください。

``` console
$ stack haddock
```
