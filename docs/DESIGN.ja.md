# Design Doc

(このドキュメントの日本語バージョン: [docs/DESIGN.ja.md](https://github.com/kmyk/Jikka/blob/master/docs/DESIGN.ja.md))

## Objective

Jikka automates the very process of solving problems of competitive programming.

## Goals

- Automatically generating solutions to competition programming problems given in a formal form
- Mechanically processing some implementation parts of competition programming
  - In practical use, it aims for a position similar to [OEIS](https://oeis.org/) and [Wolfram&#x7c;Alpha](https://www.wolframalpha.com/).

## Non-Goals

- Automatically generating solutions to competition programming problems given in natural language
  - Let GPT-3, GitHub Copilot, etc. do this for you.

## Background

There are some problems in competitive programming that can be solved mechanically.
Even if the whole problem cannot be solved mechanically, there are many problems that can be solved partially mechanically.
For example, there are problems that can be solved by carefully transforming the given formula, which each transformation itself is trivial.
We want to automate this.

## Overview

Jikka is an automatic solver for problems of competitive programming.
We can think many possible forms of automatic solvers, but Jikka is implemented especially as a transpiler doing optimization.
In other words, Jikka is just a transpiler in its overview.
It takes source code of a very restricted subset of Python as input, optimizes it in an internal language similar to GHC's Core, and finally writes source code of C++ as output.

In addition, Jikka can be used as an IDE plugin that provides rewriting functions for optimization, for more practical use during real contests.
For example, by right-clicking on a snippet of code and selecting "Rewrite this O(N²) loop to O(N)" from the menu, the code will be rewritten as such.
It is implemented using the Language Server Protocol, and supports both Python and C++ languages.

## Detailed Design

### Implemented as a transpiler

Jikka is implemented as a transpiler, because it is easy to develop and use.
The goal of a solver for competitive programming problems is too ambitious and vague to create a perfect one from the beginning.
Therefore, we should start with an easy-to-implement and already well-understood thing, i.e., a transpiler.

### Implemented as IDE plugin

Jikka is also available as an IDE plugin.
This makes it easier to use in practice.
With a transpiler that converts a large file to a large file, it is difficult to understand what kind of optimization has been done.
On the other hand, with the IDE's function that rewrites a small snippet of code into a small snippet of code, it is easier for the user to understand and control what kind of optimization has been done.

### Use Python for input

We will use a subset of Python as input, due to the following two reasons:

1. easy to use: easy to learn and write; Python is a widely used and reasonably expensive language.
2. easy to develop: easy to handle with a compiler, if we limit the features. The differences from real Python can be absorbed in the form of undefined behaviors.

We largely limit the language features, and makes it a statically typed language.
Side effects are left only as some kind of syntax suger, and removed in its core semantics.

### Don't use a new own language for input

We avoid using a new own language as input, due to the following two reasons.
Each of these is an inverse of the reason for using Python:

1. difficult to use: A new own language requires the user to learn the language anew. For most users, learning a new language is a significant burden.
2. difficult to develop: A new own language requires the developer to carefully write documentation for the language. Clear specifications and plenty of explanations are one of the important features of a programming language.

### Use C++ for output

There are two reasons:

1. ease of use: flexible in the context of competitive programming
2. easy to develop: no need to worry about constant-factor optimization

## Metrics Considerations

Under appropriate assumptions, it is possible to evaluate performance in terms of rating on AtCoder.
It may also be possible to evaluate performance in terms of how many of the problems in the AtCoder Beginner Contest can be solved.

## Testing Plan

In competition programming, it is easy to write fast and stable end-to-end tests, so we use them.
This is a form of testing that is commonly referred to as "verifying", in which you check the AC by using it against real problems of competition programming.

There are also dedicated online judges for this purpose: <https://judge.kimiyuki.net/>
