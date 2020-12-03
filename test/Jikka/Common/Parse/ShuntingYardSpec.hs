{-# LANGUAGE OverloadedStrings #-}

module Jikka.Common.Parse.ShuntingYardSpec
  ( spec,
  )
where

import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Jikka.Common.Language.Pos
import Jikka.Common.Parse.ShuntingYard (BinOpInfo (..), Fixity (..), run)
import Test.Hspec

-- Haskell's one
builtInOps :: M.Map String BinOpInfo
builtInOps =
  let op fixity prec name = (name, BinOpInfo fixity prec)
   in M.fromList
        [ op Rightfix 8 "**",
          op Leftfix 7 "*",
          op Leftfix 7 "/",
          op Leftfix 7 "%",
          op Leftfix 6 "+",
          op Leftfix 6 "-",
          op Nonfix 4 "==",
          op Nonfix 4 "/=",
          op Nonfix 4 "<",
          op Nonfix 4 "<=",
          op Nonfix 4 ">",
          op Nonfix 4 ">=",
          op Rightfix 3 "&&",
          op Rightfix 2 "||"
        ]

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing = Left a
maybeToEither _ (Just b) = Right b

run' :: [String] -> Either String String
run' tokens = value <$> run info apply (f (map putPos tokens))
  where
    info op = maybeToEither (show op ++ " is not defined") $ M.lookup op builtInOps
    apply op x y = putPos $ "(" ++ value x ++ " " ++ value op ++ " " ++ value y ++ ")"
    f [] = error "the length of tokens must be odd"
    f [z] = (z, [])
    f (x : y : zs) = let (z, ws) = f zs in (x, (y, z) : ws)
    putPos = WithPos (Pos 0 0)

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let tokens = ["a", "+", "b", "**", "c", "*", "d"]
    let tree = "(a + ((b ** c) * d))"
    run' tokens `shouldBe` Right tree
  it "recognizes left-fixity" $ do
    let tokens = ["a", "-", "b", "-", "c", "-", "d"]
    let tree = "(((a - b) - c) - d)"
    run' tokens `shouldBe` Right tree
  it "recognizes right-fixity" $ do
    let tokens = ["a", "&&", "b", "&&", "c", "&&", "d"]
    let tree = "(a && (b && (c && d)))"
    run' tokens `shouldBe` Right tree
  it "reports the error on chained non-fix ops" $ do
    let tokens = ["a", "==", "b", "==", "c"]
    run' tokens `shouldSatisfy` isLeft
