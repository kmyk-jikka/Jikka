#!/bin/bash
set -ex
tempdir=$(mktemp -d)
trap "rm -rf $tempdir" EXIT
for input in examples/*.*.in ; do
    output=${input%.in}.out
    code=${input%.*.in}.py
    name=$(basename ${input%.*.in})
    if [[ ! $input =~ large ]] ; then
        diff <(stack --system-ghc run -- execute --target rpython $code < $input) $output
    fi
    diff <(stack --system-ghc run -- execute --target core $code < $input) $output
    stack --system-ghc run -- convert --target cxx $code > $tempdir/$name.cpp
    g++ -std=c++17 -Wall -O2 -Iruntime/include $tempdir/$name.cpp -o $tempdir/$name
    diff <($tempdir/$name < $input) $output
done
