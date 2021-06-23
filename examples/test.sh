#!/bin/bash
set -ex
tempdir=$(mktemp -d)
trap "rm -rf $tempdir" EXIT
for f in examples/*.in ; do
    diff <(stack --system-ghc run -- execute --target rpython ${f%.in}.py < $f) ${f%.in}.out
    diff <(stack --system-ghc run -- execute --target core ${f%.in}.py < $f) ${f%.in}.out
    stack --system-ghc run -- convert --target cxx ${f%.in}.py > $tempdir/$(basename $f .in).cpp
    g++ -std=c++17 -Wall -O2 -Iruntime/include $tempdir/$(basename $f .in).cpp -o $tempdir/$(basename $f .in)
    diff <($tempdir/$(basename $f .in) < $f) ${f%.in}.out
done
