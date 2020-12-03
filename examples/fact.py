from jikka.compat import *

def f(n: nat) -> nat:
    if n == 0:
        return 1
    else:
        return n * f(n - 1)
