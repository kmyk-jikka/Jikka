module Jikka.Common.Combinatorics where

fact :: Integral a => a -> a
fact n | n < 0 = error "Jikka.Common.Combinatorics.fact: invalid argument"
fact n = product [1 .. n]

choose :: Integral a => a -> a -> a
choose n r | not (0 <= r && r <= n) = error "Jikka.Common.Combinatorics.choose: invalid argument"
choose n r = product [n - r + 1 .. n] `div` product [1 .. r]

permute :: Integral a => a -> a -> a
permute n r | not (0 <= r && r <= n) = error "Jikka.Common.Combinatorics.permute: invalid argument"
permute n r = product [n - r + 1 .. n]

multichoose :: Integral a => a -> a -> a
multichoose n r | not (0 <= r && r <= n) = error "Jikka.Common.Combinatorics.multichoose: invalid argument"
multichoose 0 0 = 1
multichoose n r = choose (n + r - 1) r
