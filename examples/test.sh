#!/bin/bash
set -ex
for f in examples/*.in ; do
    diff <(stack --system-ghc run -- execute --target rpython ${f%.in}.py < $f) ${f%.in}.out
    # diff <(stack --system-ghc run -- execute --target core ${f%.in}.py < $f) ${f%.in}.out
done
