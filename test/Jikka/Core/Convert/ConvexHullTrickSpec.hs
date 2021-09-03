{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.ConvexHullTrickSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.ConstantFolding as ConstantFolding
import Jikka.Core.Convert.ConvexHullTrick (run)
import qualified Jikka.Core.Convert.TypeInfer as TypeInfer
import Jikka.Core.Format (formatProgram)
import Jikka.Core.Language.Expr
import Jikka.Core.Parse (parseProgram)
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . (ConstantFolding.run <=< run)

parseProgram' :: [String] -> Program
parseProgram' = fromSuccess . flip evalAlphaT 100 . (TypeInfer.run <=< parseProgram . unlines)

spec :: Spec
spec = describe "run" $ do
  it "works on morau-style min" $ do
    let prog =
          parseProgram'
            [ "let k: int = 1000",
              "in let a: int list = range k",
              "in let b: int list = range k",
              "in let c: int list = range k",
              "in let e: int = 1234",
              "in let xs: int list = range k",
              "in build (fun ys ->",
              "    minimum (map (fun j -> a[j] * xs[len ys] + b[j] + c[len ys]) (range (len ys)))",
              ") (snoc nil e) k"
            ]
    let step =
          unlines
            [ "fun ($7: convex_hull_trick * int list) ($8: int) ->",
              "    let ys$9: int list = $7.1",
              "    in let ys$10: int list = snoc ys$9 (cht_getmin $7.0 xs[$8 + 1] + c[$8 + 1])",
              "    in let $11: convex_hull_trick = cht_insert $7.0 a[$8 + 1] b[$8 + 1]",
              "    in ($11, ys$10)"
            ]
    let base =
          unlines
            [ "let ys$12: int list = (snoc nil e)",
              "in (foldl (fun ($13: convex_hull_trick) ($14: int) ->",
              "    cht_insert $13 a[$14] b[$14]",
              ") cht_init (range 1), ys$12)"
            ]
    let expected =
          parseProgram'
            [ "let k: int = 1000",
              "in let a: int list = range k",
              "in let b: int list = range k",
              "in let c: int list = range k",
              "in let e: int = 1234",
              "in let xs: int list = range k",
              "in (foldl (" ++ step ++ ") (" ++ base ++ ") (range k)).1",
              ""
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
  it "works on morau-style min-snoc" $ do
    let prog =
          parseProgram'
            [ "let k: int = 1000",
              "in let a: int list = range k",
              "in let b: int list = range k",
              "in let c: int list = range k",
              "in let e: int list -> int = fun f -> len f + 1234",
              "in let xs: int list = range k",
              "in build (fun ys ->",
              "    minimum (snoc (map (fun j -> a[j] * xs[len ys] + b[j] + c[len ys]) (range (len ys))) (e ys))",
              ") nil k"
            ]
    let step =
          unlines
            [ "fun ($7: convex_hull_trick * int list) ($8: int) ->",
              "    let ys$9: int list = $7.1",
              "    in let ys$10: int list = snoc ys$9 (min (e ys$9 - c[$8 + 1]) (cht_getmin $7.0 xs[$8 + 1]) + c[$8 + 1])",
              "    in let $11: convex_hull_trick = cht_insert $7.0 a[$8 + 1] b[$8 + 1]",
              "    in ($11, ys$10)"
            ]
    let base =
          unlines
            [ "let ys$13: int list = snoc nil ((let ys$12: int list = nil in e ys$12 - c[0]) + c[0])",
              "in (foldl (fun ($14: convex_hull_trick) ($15: int) ->",
              "    cht_insert $14 a[$15] b[$15]",
              ") cht_init (range 1), ys$13)"
            ]
    let expected =
          parseProgram'
            [ "let k: int = 1000",
              "in let a: int list = range k",
              "in let b: int list = range k",
              "in let c: int list = range k",
              "in let e: int list -> int = fun f -> len f + 1234",
              "in let xs: int list = range k",
              "in (foldl (" ++ step ++ ") (" ++ base ++ ") (range (k - 1))).1",
              ""
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
