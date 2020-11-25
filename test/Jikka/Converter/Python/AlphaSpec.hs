module Jikka.Converter.Python.AlphaSpec (spec) where

import Data.Either (isLeft)
import Jikka.Converter.Python.Alpha (run)
import Jikka.Language.Common.Name
import Jikka.Language.Common.Pos
import Jikka.Language.Python.Parsed.Type
import Test.Hspec

at :: a -> Int -> WithPos a
at a x = WithPos (Pos 0 x) a

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let parsed =
          Program
            [ FunDef
                (FunName "solve")
                [(VarName "x", Nothing)]
                Nothing
                [ Define (VarName "y") Nothing (Var (VarName "x") `at` 2) `at` 3
                ]
                `at` 1
            ]
    let expected =
          Program
            [ FunDef
                (FunName "solve@0")
                [(VarName "x@1", Nothing)]
                Nothing
                [ Define (VarName "y@2") Nothing (Var (VarName "x@1") `at` 2) `at` 3
                ]
                `at` 1
            ]
    run parsed `shouldBe` Right expected
  it "distinguishes local variables in two diffrent functions" $ do
    let parsed =
          Program
            [ FunDef
                (FunName "foo")
                [(VarName "x", Nothing)]
                Nothing
                [ Define (VarName "y") Nothing (Var (VarName "x") `at` 2) `at` 3
                ]
                `at` 1,
              FunDef
                (FunName "bar")
                [(VarName "x", Nothing)]
                Nothing
                [ Define (VarName "y") Nothing (Var (VarName "x") `at` 5) `at` 6
                ]
                `at` 4
            ]
    let expected =
          Program
            [ FunDef
                (FunName "foo@0")
                [(VarName "x@1", Nothing)]
                Nothing
                [ Define (VarName "y@2") Nothing (Var (VarName "x@1") `at` 2) `at` 3
                ]
                `at` 1,
              FunDef
                (FunName "bar@3")
                [(VarName "x@4", Nothing)]
                Nothing
                [ Define (VarName "y@5") Nothing (Var (VarName "x@4") `at` 5) `at` 6
                ]
                `at` 4
            ]
    run parsed `shouldBe` Right expected
  it "distinguishes variables in two diffrent for-loops" $ do
    let parsed =
          Program
            [ FunDef
                (FunName "solve")
                []
                Nothing
                [ For
                    (VarName "i")
                    (ListExt [] `at` 3)
                    [ Define (VarName "x") Nothing (Var (VarName "i") `at` 5) `at` 4
                    ]
                    `at` 2,
                  For
                    (VarName "i")
                    (ListExt [] `at` 7)
                    [ Define (VarName "x") Nothing (Var (VarName "i") `at` 9) `at` 8
                    ]
                    `at` 6
                ]
                `at` 1
            ]
    let expected =
          Program
            [ FunDef
                (FunName "solve@0")
                []
                Nothing
                [ For
                    (VarName "i@1")
                    (ListExt [] `at` 3)
                    [ Define (VarName "x@2") Nothing (Var (VarName "i@1") `at` 5) `at` 4
                    ]
                    `at` 2,
                  For
                    (VarName "i@3")
                    (ListExt [] `at` 7)
                    [ Define (VarName "x@4") Nothing (Var (VarName "i@3") `at` 9) `at` 8
                    ]
                    `at` 6
                ]
                `at` 1
            ]
    run parsed `shouldBe` Right expected
  it "removes underscoes" $ do
    let parsed =
          Program
            [ ConstDef
                (VarName "a")
                Nothing
                ( ListComp
                    ( Comprehension
                        (Lit (LitInt 0) `at` 5)
                        Nothing
                        (Call (FunName "range") [Lit (LitInt 10) `at` 4] `at` 3)
                        Nothing
                    )
                    `at` 2
                )
                `at` 1
            ]
    let expected =
          Program
            [ ConstDef
                (VarName "a@0")
                Nothing
                ( ListComp
                    ( Comprehension
                        (Lit (LitInt 0) `at` 5)
                        (Just (VarName "_@1"))
                        (Call (FunName "range") [Lit (LitInt 10) `at` 4] `at` 3)
                        Nothing
                    )
                    `at` 2
                )
                `at` 1
            ]
    run parsed `shouldBe` Right expected
  it "works on recursive functions" $ do
    let parsed =
          Program
            [ FunDef
                (FunName "f")
                [(VarName "x", Nothing)]
                Nothing
                [ Return (Call (FunName "f") [Var (VarName "x") `at` 4] `at` 3) `at` 2
                ]
                `at` 1
            ]
    let expected =
          Program
            [ FunDef
                (FunName "f@0")
                [(VarName "x@1", Nothing)]
                Nothing
                [ Return (Call (FunName "f@0") [Var (VarName "x@1") `at` 4] `at` 3) `at` 2
                ]
                `at` 1
            ]
    run parsed `shouldBe` Right expected
  it "doesn't use the same name as a function and as a variable" $ do
    let parsed =
          Program
            [ FunDef
                (FunName "f")
                []
                Nothing
                [ Define (VarName "f") Nothing (Lit (LitInt 0) `at` 3) `at` 2
                ]
                `at` 1
            ]
    run parsed `shouldSatisfy` isLeft
