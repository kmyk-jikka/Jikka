from jikka.compat import *

def fact(n: nat) -> nat:
    if n == 0:
        return 1
    else:
        return n * fact(n - 1)
