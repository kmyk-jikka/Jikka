{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Jikka.Core.Language.AssertedHint
  ( AssertedHint (..),
    pattern EqualHint,
    parseHints,

    -- * Functions using hints
    lowerBoundWithHints,
    upperBoundWithHints,
    isZeroWithHints,
    nullWithHints,
    lengthWithHints,
  )
where

import Control.Monad
import Data.Semigroup
import Jikka.Core.Language.ArithmeticExpr
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr

data AssertedHint
  = -- | @NotEqualHint n@ for a integer variable @x@ means @x /= n@
    NotEqualHint ArithmeticExpr
  | -- | @BoundHint l r@ for a integer variable @x@ means @l <= x < r@
    BoundHint (Maybe ArithmeticExpr) (Maybe ArithmeticExpr)
  | -- | @LengthHint t hint@ for a list variable @xs@ means @length xs@ satisfies @hint@
    LengthHint Type AssertedHint
  | -- | @AllHint t hint@ for a list variable @xs@ means that @x@ satisfies @hint@ for all @x@ in @xs@
    AllHint Type AssertedHint
  deriving (Eq, Ord, Show)

pattern EqualHint :: ArithmeticExpr -> AssertedHint
pattern EqualHint l <-
  (\case BoundHint (Just l) (Just r) | incrArithmeticExpr l == r -> Just l; _ -> Nothing -> Just l)
  where
    EqualHint l = BoundHint (Just l) (Just (incrArithmeticExpr l))

parseNilHint :: Expr -> [(VarName, AssertedHint)]
parseNilHint = \case
  -- Jikka.Core.Language.EqualitiySolving makes rhs nil
  Equal' _ (Var xs) (Nil' t) -> [(xs, LengthHint t (BoundHint (Just (integerArithmeticExpr 0)) (Just (integerArithmeticExpr 1))))]
  NotEqual' _ (Var xs) (Nil' t) -> [(xs, LengthHint t (BoundHint (Just (integerArithmeticExpr 1)) Nothing))]
  _ -> []

data Term
  = VarTerm VarName
  | LenTerm Type VarName
  | NegatedTerm Term
  deriving (Eq, Ord, Show, Read)

-- `decomposeArithmeticExpr` makes @a + b + c@ to @[(a, b + c), (b, a + c), (c, a + b)]@ in a convenient way, because Jikka.Core.Language.EqualitiySolving makes @a == b@ to @a - b == 0@.
decomposeArithmeticExpr :: Expr -> [(Term, ArithmeticExpr)]
decomposeArithmeticExpr e =
  let es = splitToSumArithmeticExpr $ parseArithmeticExpr e
   in (`concatMap` [0 .. length es - 1]) $ \i ->
        let e = es !! i
            e' = sumArithmeticExpr $ take i es ++ drop (i + 1) es
         in case formatArithmeticExpr e of
              Var x -> [(VarTerm x, e')]
              Negate' (Var x) -> [(NegatedTerm (VarTerm x), e')]
              Len' t (Var x) -> [(LenTerm t x, e')]
              Negate' (Len' t (Var x)) -> [(NegatedTerm (LenTerm t x), e')]
              _ -> []

parseBoundHint :: Expr -> [(VarName, AssertedHint)]
parseBoundHint = \case
  Equal' IntTy e (LitInt' 0) -> do
    (x, e) <- decomposeArithmeticExpr e
    case x of
      VarTerm x -> return (x, EqualHint (negateArithmeticExpr e))
      NegatedTerm (VarTerm x) -> return (x, EqualHint e)
      LenTerm t x -> return (x, LengthHint t (EqualHint (negateArithmeticExpr e)))
      NegatedTerm (LenTerm t x) -> return (x, LengthHint t (EqualHint e))
      _ -> []
  NotEqual' IntTy e (LitInt' 0) -> do
    (x, e) <- decomposeArithmeticExpr e
    case x of
      VarTerm x -> return (x, NotEqualHint (negateArithmeticExpr e))
      NegatedTerm (VarTerm x) -> return (x, NotEqualHint e)
      LenTerm t x | isZeroArithmeticExpr e -> return (x, LengthHint t (BoundHint (Just (integerArithmeticExpr 1)) Nothing))
      NegatedTerm (LenTerm t x) | isZeroArithmeticExpr e -> return (x, LengthHint t (BoundHint (Just (integerArithmeticExpr 1)) Nothing))
      _ -> []
  LessEqual' IntTy e (LitInt' 0) -> do
    (x, e) <- decomposeArithmeticExpr e
    case x of
      VarTerm x -> return (x, BoundHint Nothing (Just (incrArithmeticExpr (negateArithmeticExpr e))))
      NegatedTerm (VarTerm x) -> return (x, BoundHint (Just e) Nothing)
      LenTerm t x -> return (x, LengthHint t (BoundHint Nothing (Just (incrArithmeticExpr (negateArithmeticExpr e)))))
      NegatedTerm (LenTerm t x) -> return (x, LengthHint t (BoundHint (Just e) Nothing))
      _ -> []
  LessThan' IntTy e (LitInt' 0) -> do
    (x, e) <- decomposeArithmeticExpr e
    case x of
      VarTerm x -> return (x, BoundHint Nothing (Just (negateArithmeticExpr e)))
      NegatedTerm (VarTerm x) -> return (x, BoundHint (Just (incrArithmeticExpr e)) Nothing)
      LenTerm t x -> return (x, LengthHint t (BoundHint Nothing (Just (negateArithmeticExpr e))))
      NegatedTerm (LenTerm t x) -> return (x, LengthHint t (BoundHint (Just (incrArithmeticExpr e)) Nothing))
      _ -> []
  -- Jikka.Core.Language.EqualitiySolving makes rhs 0 and removes GreaterEqual and GreaterThan
  _ -> []

parseAndHint :: Expr -> [(VarName, AssertedHint)]
parseAndHint = \case
  And' e e' -> parseHints e ++ parseHints e'
  _ -> []

parseAllHint :: Expr -> [(VarName, AssertedHint)]
parseAllHint = \case
  All' (Map' t _ (Lam x _ pred) (Var xs)) -> do
    (x', hint) <- parseHints pred
    guard $ x' == x
    return (xs, AllHint t hint)
  _ -> []

parseHints :: Expr -> [(VarName, AssertedHint)]
parseHints e =
  concat
    [ parseNilHint e,
      parseBoundHint e,
      parseAndHint e,
      parseAllHint e
    ]

selectHints :: [(VarName, AssertedHint)] -> VarName -> [AssertedHint]
selectHints hints x = map snd (filter (\hint -> fst hint == x) hints)

maximum' :: Ord a => [Maybe a] -> Maybe a
maximum' = (getMax <$>) . mconcat . map (Max <$>)

minimum' :: Ord a => [Maybe a] -> Maybe a
minimum' = (getMin <$>) . mconcat . map (Min <$>)

lowerBoundWithHints :: [(VarName, AssertedHint)] -> Expr -> Maybe Integer
lowerBoundWithHints hints = \case
  LitInt' n -> Just n
  Negate' e -> do
    e <- upperBoundWithHints hints e
    return $ - (e - 1)
  Plus' e1 e2 -> do
    e1 <- lowerBoundWithHints hints e1
    e2 <- lowerBoundWithHints hints e2
    return $ e1 + e2
  Minus' e1 e2 -> do
    e1 <- lowerBoundWithHints hints e1
    e2 <- upperBoundWithHints hints e2
    return $ e1 - (e2 - 1)
  Mult' e1 e2 -> do
    e1 <- lowerBoundWithHints hints e1
    e2 <- lowerBoundWithHints hints e2
    guard $ e1 >= 0 && e2 >= 0
    return $ e1 * e2
  Var x ->
    let go :: AssertedHint -> Maybe Integer
        go = \case
          BoundHint (Just l) _ -> lowerBoundWithHints hints (formatArithmeticExpr l)
          _ -> Nothing
     in maximum' (map go (selectHints hints x))
  _ -> Nothing

upperBoundWithHints :: [(VarName, AssertedHint)] -> Expr -> Maybe Integer
upperBoundWithHints hints = \case
  LitInt' n -> Just (n + 1)
  Negate' e -> do
    e <- lowerBoundWithHints hints e
    return $ - e + 1
  Plus' e1 e2 -> do
    e1 <- upperBoundWithHints hints e1
    e2 <- upperBoundWithHints hints e2
    return $ e1 + e2 - 1
  Minus' e1 e2 -> do
    e1 <- upperBoundWithHints hints e1
    e2 <- lowerBoundWithHints hints e2
    return $ e1 - e2
  Mult' e1 e2 -> do
    l1 <- lowerBoundWithHints hints e1
    l2 <- lowerBoundWithHints hints e2
    r1 <- upperBoundWithHints hints e1
    r2 <- upperBoundWithHints hints e2
    guard $ l1 >= 0 && l2 >= 0
    return $ (r1 - 1) * (r2 - 1) + 1
  Var x ->
    let go :: AssertedHint -> Maybe Integer
        go = \case
          BoundHint _ (Just r) -> pred <$> upperBoundWithHints hints (formatArithmeticExpr r)
          _ -> Nothing
     in minimum' (map go (selectHints hints x))
  _ -> Nothing

isZeroWithHints :: [(VarName, AssertedHint)] -> Expr -> Maybe Bool
isZeroWithHints hints e =
  case (lowerBoundWithHints hints e, upperBoundWithHints hints e) of
    (Just 0, Just 1) -> Just True
    (Just l, _) | l >= 1 -> Just False
    (_, Just r) | r <= 0 -> Just False
    _ -> Nothing

integerWithHints :: [(VarName, AssertedHint)] -> Expr -> Maybe Integer
integerWithHints hints = \case
  LitInt' n -> Just n
  Negate' e -> do
    e <- integerWithHints hints e
    return $ - e
  Plus' e1 e2 -> do
    e1 <- integerWithHints hints e1
    e2 <- integerWithHints hints e2
    return $ e1 + e2
  Minus' e1 e2 -> do
    e1 <- integerWithHints hints e1
    e2 <- integerWithHints hints e2
    return $ e1 - e2
  Mult' e1 e2 -> do
    e1 <- integerWithHints hints e1
    e2 <- integerWithHints hints e2
    return $ e1 * e2
  e@(Var _) ->
    case (lowerBoundWithHints hints e, upperBoundWithHints hints e) of
      (Just l, Just r) | l + 1 == r -> Just l
      _ -> Nothing
  _ -> Nothing

nullWithHints :: [(VarName, AssertedHint)] -> Expr -> Maybe Bool
nullWithHints hints = \case
  Nil' _ -> Just True
  Cons' _ _ _ -> Just False
  Range1' n -> isZeroWithHints hints n
  Var xs -> do
    let go :: AssertedHint -> Maybe Integer
        go = \case
          LengthHint _ (BoundHint (Just l) _) -> lowerBoundWithHints hints (formatArithmeticExpr l)
          _ -> Nothing
    l <- maximum' (map go (selectHints hints xs))
    if l >= 1
      then Just False
      else do
        let go :: AssertedHint -> Maybe Integer
            go = \case
              LengthHint _ (NotEqualHint n) -> integerWithHints hints (formatArithmeticExpr n)
              _ -> Nothing
        let ns = map go (selectHints hints xs)
        if Just 0 `elem` ns
          then Just False
          else do
            let go :: AssertedHint -> Maybe Integer
                go = \case
                  LengthHint _ (BoundHint _ (Just r)) -> pred <$> upperBoundWithHints hints (formatArithmeticExpr r)
                  _ -> Nothing
            r <- minimum' (map go (selectHints hints xs))
            if r <= 1
              then Just True
              else Nothing
  _ -> Nothing

lengthWithHints :: [(VarName, AssertedHint)] -> Expr -> Maybe Integer
lengthWithHints hints = \case
  Nil' _ -> Just 0
  Cons' _ _ xs -> do
    n <- lengthWithHints hints xs
    return $ n + 1
  Range1' e -> case (lowerBoundWithHints hints e, upperBoundWithHints hints e) of
    (Just l, Just r) | l + 1 == r -> Just l
    _ -> Nothing
  Var xs -> do
    let go :: AssertedHint -> Maybe Integer
        go = \case
          LengthHint _ (BoundHint (Just l) _) -> lowerBoundWithHints hints (formatArithmeticExpr l)
          _ -> Nothing
    let minimum' :: Ord a => [Maybe a] -> Maybe a
        minimum' = (getMin <$>) . mconcat . map (Min <$>)
    minimum' (map go (selectHints hints xs))
  _ -> Nothing
