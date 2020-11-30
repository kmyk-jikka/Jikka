module Jikka.Converter.Core.MakeEagerSpec (spec) where

import Jikka.Converter.Core.MakeEager (run)
import Jikka.Language.Core.Expr
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let input =
          ResultExpr
            ( AppBuiltin
                (If IntTy)
                [ LitTrue,
                  Lit0,
                  Lit1
                ]
            )
    let expected =
          ResultExpr
            ( App
                ( AppBuiltin
                    (If (FunTy [] IntTy))
                    [ LitTrue,
                      Lam [] Lit0,
                      Lam [] Lit1
                    ]
                )
                []
            )
    run input `shouldBe` Right expected
