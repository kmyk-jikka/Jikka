# Design Doc

[#7](https://github.com/kmyk/Jikka/pull/7) も読んでね


## Objective

Jikka は、競技プログラミングの問題を解く行為そのものを自動化するツールである。
特にこれを「最適化コンパイラ」あるいは「自動定理証明」の形に落とし込んで実現する。

## Goals

-   形式的な形で与えられた競技プログラミングの問題の解法を自動で生成すること。またこれによりユーザのレートを上げること
-   競技プログラミングにおける機械的に処理できる部分を機械的に処理することによって、機械的には処理できないより本質的でおもしろい部分に人間が集中できるようにすること


## Non-Goals

-   自然言語で与えられた競技プログラミングの問題の解法を自動で生成すること
-   競技プログラミングにおける機械的に処理できる部分を機械的に処理することによって、競技プログラミングという遊びを spoil すること


## Background

競技プログラミングの問題を解く過程は以下の 3 ステップに大別できる。

1.  形式化: 自然言語で与えられた問題を解釈し、数学的で形式的な問題として整理する
2.  考察: 数学的で形式的な問題に対し、数学的で抽象的な解法を考案する
3.  実装: 数学的で抽象的な解法を、計算機における具体的な実装として記述する

競技プログラミングの (すくなくとも AtCoder で出題されるような問題の) 中心であり最もおもしろくかつ難しい部分は「考察」の部分であると考えられている。
しかしこの「考察」にも、一定の知識があれば機械的に可能であるような部分は多く含まれている。
Jikka はそのような機械的な考察を処理する。


## Overview

Jikka はその概観としてはただのトランスパイラである。
ML 風言語や Haskell 風言語や C++ 風言語で書かれたソースコードを受けとり、これを高級内部言語 `Sugared` として解釈し、これを低級内部言語 `Core` に変換して最適化し、ML 風言語や Haskell 風言語や C++ 風言語として出力する。
最適化は低級内部言語のレベルで行われる。

TODO: もうすこし書く


## Detailed Design

TODO: 書く

### ML-like language


### Sugared language

Sugared 言語は、内部で用いられる高級言語である。
ML を参考にしている。

```
<program>
    ::= "let" "given" <ident> ":" <type> "in" <program>
      | <expr>

<expr>
    ::= <item>
      | <expr> <expr>
      | <expr> <binop> <expr>
      | "let" "rec" <ident> [":" <type>] "=" <expr> "in" <expr>
      | "let" <ident> [":" <type>] "=" <expr> "in" <expr>
      | "if" <expr> "then" <expr> "else" <expr>
      | "match" <expr> "with" <match-branches> "end"
      | "function" <match-branches> "end"
      | "fun" <args> "->" <expr>

<match-branches>
    ::= "|" <pattern> "->" <expr>

<item>
    ::= <ident>
      | <literal>
      | "(" <expr> ")"

<type>
    ::= <ident>
      | <type> "->" <type>
      | "(" <type> ")"
```


### Core language

Core 言語は、内部で用いられる低級言語である。
GHC Core を参考にしている。

```
<program>
    ::= "let" <bind> "in" <program>
      | <expr>

<bind>
    ::= "given" <ident> ":" <type>
      | "rec" <ident> ":" <type> "=" <expr>
      | <ident> ":" <type> "=" <expr>

<expr>
    ::= <ident>
      | <literal>
      | <expr> <expr>
      | "\\" <var> ":" <type> "->" <expr>
      | "let" <ident> ":" <type> "=" <expr> "in" <expr>
      | "case" <expr> "of" "{" <match-branches> "}"

<match-branch>
    ::= <pattern> "->" <expr>

<item>
    ::= <ident>
      | <literal>
      | "(" <expr> ")"

<type>
    ::= <ident>
      | <type> <type>
      | <type> "->" <type>
      | "forall" <ident> "." <type>
      | "(" <type> ")"
```


## Security Considerations

特になし。


## Privacy Considerations

特になし。


## Metrics Considerations

AtCoder Beginner Contest などの問題をどの程度の割合で解けるか数えれば指標とできる。


## Testing Plan

TODO: 書く
