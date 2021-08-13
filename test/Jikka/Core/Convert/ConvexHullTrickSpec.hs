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
parseProgram' = fromSuccess . flip evalAlphaT 0 . (TypeInfer.run <=< parseProgram . unlines)

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
            [ "fun ($4: convex_hull_trick * int list) ($0: int) ->",
              "    let ys: int list = $4.1",
              "    in let ys$6: int list = snoc ys (cht_getmin $4.0 xs[$0 + 1] + c[$0 + 1])",
              "    in let $5: convex_hull_trick = cht_insert $4.0 a[$0 + 1] b[$0 + 1]",
              "    in ($5, ys$6)"
            ]
    let base =
          unlines
            [ "let ys$2: int list = (snoc nil e)",
              "in (foldl (fun ($1: convex_hull_trick) ($3: int) ->",
              "    cht_insert $1 a[$3] b[$3]",
              ") cht_init (range 1), ys$2)"
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
            [ "fun ($4: convex_hull_trick * int list) ($0: int) ->",
              "    let ys: int list = $4.1",
              "    in let ys$6: int list = snoc ys (min (e ys - c[$0 + 1]) (cht_getmin $4.0 xs[$0 + 1]) + c[$0 + 1])",
              "    in let $5: convex_hull_trick = cht_insert $4.0 a[$0 + 1] b[$0 + 1]",
              "    in ($5, ys$6)"
            ]
    let base =
          unlines
            [ "let ys$2: int list = snoc nil ((let ys: int list = nil in e ys - c[0]) + c[0])",
              "in (foldl (fun ($1: convex_hull_trick) ($3: int) ->",
              "    cht_insert $1 a[$3] b[$3]",
              ") cht_init (range 1), ys$2)"
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
