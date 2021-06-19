#!/bin/bash
set -ex
for f in examples/*.in ; do
    diff <(stack run execute ${f%.in}.py < $f) ${f%.in}.out
done
