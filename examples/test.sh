#!/bin/bash
set -ex
tempdir=$(mktemp -d)
trap "rm -rf $tempdir" EXIT
for code in examples/*.py ; do
    name=$(basename ${code%.py})
    stack --system-ghc run -- convert --target cxx $code > $tempdir/$name.cpp
    g++ -std=c++17 -Wall -O2 -Iruntime/include $tempdir/$name.cpp -o $tempdir/$name
    for input in examples/data/$name.*.in ; do
        output=${input%.in}.out
        if [[ ! $input =~ large ]] ; then
            diff <(stack --system-ghc run -- execute --target rpython $code < $input) $output
        fi
        diff <(stack --system-ghc run -- execute --target core $code < $input) $output
        diff <($tempdir/$name < $input) $output
    done
done
