module Jikka.Common.ModInt
  ( ModInt,
    toModInt,
    fromModInt,
    moduloOfModInt,
  )
where

import Data.Monoid

data ModInt = ModInt Integer (Maybe Integer)
  deriving (Eq, Ord, Read, Show)

toModInt :: Integer -> Integer -> ModInt
toModInt _ m | m <= 0 = error $ "Jikka.Common.ModInt.toModInt: modulo must be positive, but m = " ++ show m
toModInt a m = ModInt (a `mod` m) (Just m)

fromModInt :: ModInt -> Integer
fromModInt (ModInt a _) = a

moduloOfModInt :: ModInt -> Maybe Integer
moduloOfModInt (ModInt _ m) = m

instance Num ModInt where
  ModInt _ (Just m1) + ModInt _ (Just m2) | m1 /= m2 = error $ "Jikka.Common.ModInt.(+): modulo must be the same, but m1 = " ++ show m1 ++ " and m2 = " ++ show m2
  ModInt a m1 + ModInt b m2 = case getFirst (First m1 <> First m2) of
    Nothing -> ModInt (a + b) Nothing
    Just m -> ModInt (let c = a + b in if c >= m then c - m else c) (Just m)
  ModInt _ (Just m1) * ModInt _ (Just m2) | m1 /= m2 = error $ "Jikka.Common.ModInt.(*): modulo must be the same, but m1 = " ++ show m1 ++ " and m2 = " ++ show m2
  ModInt a m1 * ModInt b m2 = case getFirst (First m1 <> First m2) of
    Nothing -> ModInt (a * b) Nothing
    Just m -> ModInt ((a * b) `mod` m) (Just m)
  abs = error "Jikka.Common.ModInt.fromInteger: cannot call abs for modint"
  signum = error "Jikka.Common.ModInt.fromInteger: cannot signum for modint"
  fromInteger a = ModInt a Nothing
  negate (ModInt a m) = case m of
    Nothing -> ModInt (- a) m
    Just m -> ModInt (if a == 0 then 0 else m - a) (Just m)
