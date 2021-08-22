{-# LANGUAGE OverloadedStrings #-}

module Jikka.Core.Convert.SegmentTreeSpec
  ( spec,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Convert.SegmentTree (run)
import qualified Jikka.Core.Convert.TypeInfer as TypeInfer
import Jikka.Core.Format (formatProgram)
import Jikka.Core.Language.Expr
import Jikka.Core.Parse (parseProgram)
import Test.Hspec

run' :: Program -> Either Error Program
run' = flip evalAlphaT 0 . run

parseProgram' :: [String] -> Program
parseProgram' = fromSuccess . flip evalAlphaT 100 . (TypeInfer.run <=< parseProgram . unlines)

spec :: Spec
spec = describe "run" $ do
  it "works on int-plus semigroup" $ do
    let prog =
          parseProgram'
            [ "let a1: int list = range 1000",
              "in let rec f (k: int): int list =", -- Remove distinction between top-level and non-top-level.
              "    foldl (fun a2 i ->",
              "        a2[i + 10 <- (scanl (fun x y -> x + y) 0 a2)[i + 100]]",
              "    ) a1 (range k)",
              "in f 100"
            ]
    let expected =
          parseProgram'
            [ "let a1$3: int list = range 1000",
              "in let rec f$4 (k$5: int): int list =",
              "    let a2$6 = a1$3",
              "    in (foldl (fun a2$7 i$8 ->",
              "        let $9 = 0 + segtree_getrange<int_plus> a2$7.1 0 (i$8 + 100)",
              "        in (a2$7.0[i$8 + 10 <- $9], segtree_setpoint<int_plus> a2$7.1 (i$8 + 10) $9)",
              "    ) (a2$6, segtree_init<int_plus> a2$6) (range k$5)).0",
              "in f$4 100"
            ]
    (formatProgram <$> run' prog) `shouldBe` Right (formatProgram expected)
