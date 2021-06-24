{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Jikka.Core.Language.ArithmeticalExpr where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List (findIndices, groupBy, sort, sortBy)
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Vars

data ProductExpr = ProductExpr
  { productExprConst :: Integer,
    productExprList :: [Expr]
  }
  deriving (Eq, Ord, Show, Read)

data SumExpr = SumExpr
  { sumExprList :: [ProductExpr],
    sumExprConst :: Integer
  }
  deriving (Eq, Ord, Show, Read)

type ArithmeticalExpr = SumExpr

oneProductExpr :: ProductExpr
oneProductExpr =
  ProductExpr
    { productExprConst = 1,
      productExprList = []
    }

negateProductExpr :: ProductExpr -> ProductExpr
negateProductExpr e = e {productExprConst = negate (productExprConst e)}

multProductExpr :: ProductExpr -> ProductExpr -> ProductExpr
multProductExpr e1 e2 =
  ProductExpr
    { productExprConst = productExprConst e1 * productExprConst e2,
      productExprList = productExprList e1 ++ productExprList e2
    }

parseProductExpr :: Expr -> ProductExpr
parseProductExpr = \case
  LitInt' n -> ProductExpr {productExprConst = n, productExprList = []}
  Negate' e -> negateProductExpr (parseProductExpr e)
  Mult' e1 e2 -> multProductExpr (parseProductExpr e1) (parseProductExpr e2)
  e -> ProductExpr {productExprConst = 1, productExprList = [e]}

sumExprFromProductExpr :: ProductExpr -> SumExpr
sumExprFromProductExpr e =
  SumExpr
    { sumExprList = [e],
      sumExprConst = 0
    }

sumExprFromInteger :: Integer -> SumExpr
sumExprFromInteger n =
  SumExpr
    { sumExprConst = n,
      sumExprList = []
    }

zeroSumExpr :: SumExpr
zeroSumExpr = sumExprFromInteger 0

negateSumExpr :: SumExpr -> SumExpr
negateSumExpr e =
  SumExpr
    { sumExprList = map negateProductExpr (sumExprList e),
      sumExprConst = negate (sumExprConst e)
    }

plusSumExpr :: SumExpr -> SumExpr -> SumExpr
plusSumExpr e1 e2 =
  SumExpr
    { sumExprList = sumExprList e1 ++ sumExprList e2,
      sumExprConst = sumExprConst e1 + sumExprConst e2
    }

multSumExpr :: SumExpr -> SumExpr -> SumExpr
multSumExpr e1 e2 =
  SumExpr
    { sumExprList =
        let es1 = parseProductExpr (LitInt' (sumExprConst e1)) : sumExprList e1
            es2 = parseProductExpr (LitInt' (sumExprConst e2)) : sumExprList e2
         in map (uncurry multProductExpr) ((,) <$> es1 <*> es2),
      sumExprConst = sumExprConst e1 * sumExprConst e2
    }

parseSumExpr :: Expr -> SumExpr
parseSumExpr = \case
  LitInt' n -> SumExpr {sumExprList = [], sumExprConst = n}
  Negate' e -> negateSumExpr (parseSumExpr e)
  Plus' e1 e2 -> plusSumExpr (parseSumExpr e1) (parseSumExpr e2)
  Minus' e1 e2 -> plusSumExpr (parseSumExpr e1) (negateSumExpr (parseSumExpr e2))
  Mult' e1 e2 -> multSumExpr (parseSumExpr e1) (parseSumExpr e2)
  e -> sumExprFromProductExpr (parseProductExpr e)

-- | `parseArithmeticalExpr` converts a given expr to a normal form \(\sum_i \prod_j e _ {i,j})\).
-- This assumes given exprs have the type \(\mathbf{int}\).
parseArithmeticalExpr :: Expr -> ArithmeticalExpr
parseArithmeticalExpr = parseSumExpr

formatProductExpr :: ProductExpr -> Expr
formatProductExpr e =
  let k = LitInt' (productExprConst e)
      k' e' = if productExprConst e == 0 then Lit0 else Mult' e' k
   in case productExprList e of
        [] -> k
        eHead : esTail -> k' (foldl Mult' eHead esTail)

formatSumExpr :: SumExpr -> Expr
formatSumExpr e =
  let k = LitInt' (sumExprConst e)
   in case sumExprList e of
        [] -> k
        eHead : esTail ->
          let op e'
                | productExprConst e' > 0 = Plus'
                | productExprConst e' < 0 = Minus'
                | otherwise = const
              go e1 e2 = op e2 e1 (formatProductExpr (e2 {productExprConst = abs (productExprConst e2)}))
              k' e'
                | sumExprConst e > 0 = Plus' e' k
                | sumExprConst e < 0 = Minus' e' k
                | otherwise = e'
           in k' (foldl go (formatProductExpr eHead) esTail)

formatArithmeticalExpr :: ArithmeticalExpr -> Expr
formatArithmeticalExpr = formatSumExpr

normalizeProductExpr :: ProductExpr -> ProductExpr
normalizeProductExpr e =
  let es =
        if productExprConst e == 0
          then []
          else sort (productExprList e)
   in e {productExprList = es}

normalizeSumExpr :: SumExpr -> SumExpr
normalizeSumExpr e =
  let cmp e1 e2 = productExprList e1 `compare` productExprList e2
      cmp' e1 e2 = cmp e1 e2 == EQ
      es = sortBy cmp (map normalizeProductExpr (sumExprList e))
      es' = groupBy cmp' es
      es'' = map (\group -> ProductExpr {productExprConst = sum (map productExprConst group), productExprList = productExprList (head group)}) es'
      es''' = filter (\e -> productExprConst e /= 0 && not (null (productExprList e))) es''
      k = sum (map (\e -> if null (productExprList e) then productExprConst e else 0) es'')
   in SumExpr
        { sumExprList = es''',
          sumExprConst = sumExprConst e + k
        }

normalizeArithmeticalExpr :: ArithmeticalExpr -> ArithmeticalExpr
normalizeArithmeticalExpr = normalizeSumExpr

-- | `makeVectorFromArithmeticalExpr` makes a vector \(f\) and a expr \(c\) from a given vector of variables \(x_0, x_1, \dots, x _ {n - 1}\) and a given expr \(e\) s.t. \(f\) and \(c\) don't have \(x_0, x_1, \dots, x _ {n - 1}\) as free variables and \(e = c + f \cdot (x_0, x_1, \dots, x _ {n - 1})\) holds.
-- This assumes given variables and exprs have the type \(\mathbf{int}\).
makeVectorFromArithmeticalExpr :: V.Vector VarName -> ArithmeticalExpr -> Maybe (V.Vector ArithmeticalExpr, ArithmeticalExpr)
makeVectorFromArithmeticalExpr xs es = runST $ do
  runMaybeT $ do
    f <- lift $ MV.replicate (V.length xs) zeroSumExpr
    c <- lift $ newSTRef (sumExprFromInteger (sumExprConst es))
    forM_ (sumExprList es) $ \e -> do
      let indices = V.imap (\i x -> map (i,) (findIndices (x `isFreeVar`) (productExprList e))) xs
      case concat (V.toList indices) of
        [] -> lift $ modifySTRef c (plusSumExpr (sumExprFromProductExpr e))
        [(i, j)] -> do
          let e' = e {productExprList = take j (productExprList e) ++ drop (j + 1) (productExprList e)}
          lift $ MV.modify f (plusSumExpr (sumExprFromProductExpr e')) i
        _ -> MaybeT $ return Nothing
    f <- V.freeze f
    c <- lift $ readSTRef c
    return (V.map normalizeArithmeticalExpr f, normalizeArithmeticalExpr c)
