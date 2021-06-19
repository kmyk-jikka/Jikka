#!/bin/bash
set -ex
for f in examples/*.in ; do
    diff <(stack --system-ghc run execute ${f%.in}.py < $f) ${f%.in}.out
done
