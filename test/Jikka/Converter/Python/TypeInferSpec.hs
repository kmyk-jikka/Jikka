module Jikka.Converter.Python.TypeInferSpec (spec) where

import Data.Either (isLeft)
import Jikka.Converter.Python.TypeInfer (run)
import Jikka.Language.Common.Name
import Jikka.Language.Common.Pos
import Jikka.Language.Python.Typed.Stdlib
import Jikka.Language.Python.Typed.Type
import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "works" $ do
    let untyped =
          Program
            [ FunDef
                (FunName "solve@0")
                [(VarName "x@1", ATyVar (TypeName "t@0"))]
                (ATyVar (TypeName "t@1"))
                [ Return (UnOp Negate (Var (VarName "x@1")))
                ]
            ]
    let typed =
          Program
            [ FunDef
                (FunName "solve@0")
                [(VarName "x@1", ATyInt)]
                ATyInt
                [ Return (UnOp Negate (Var (VarName "x@1")))
                ]
            ]
    run untyped `shouldBe` Right typed
  it "works on list comprehension" $ do
    let untyped =
          Program
            [ FunDef
                (FunName "solve@0")
                [ (VarName "xs@1", ATyVar (TypeName "t@0")),
                  (VarName "y@2", ATyVar (TypeName "t@1"))
                ]
                (ATyVar (TypeName "t@2"))
                [ Return
                    ( UnOp
                        Sum
                        ( IterComp
                            ( Comprehension
                                (ATyVar (TypeName "t@3"))
                                (Var (VarName "y@2"))
                                (VarName "x@3")
                                (ATyVar (TypeName "t@4"))
                                (Var (VarName "xs@1"))
                                (Just (Var (VarName "x@3")))
                            )
                        )
                    )
                ]
            ]
    let typed =
          Program
            [ FunDef
                (FunName "solve@0")
                [ (VarName "xs@1", ATyIterator ATyBool),
                  (VarName "y@2", ATyInt)
                ]
                ATyInt
                [ Return
                    ( UnOp
                        Sum
                        ( IterComp
                            ( Comprehension
                                ATyInt
                                (Var (VarName "y@2"))
                                (VarName "x@3")
                                ATyBool
                                (Var (VarName "xs@1"))
                                (Just (Var (VarName "x@3")))
                            )
                        )
                    )
                ]
            ]
    run untyped `shouldBe` Right typed
  it "allows to use a list as an iterator" $ do
    let untyped =
          Program
            [ FunDef
                (FunName "solve@0")
                [ (VarName "xs@1", ATyList (ATyVar (TypeName "t@0")))
                ]
                (ATyVar (TypeName "t@1"))
                [ Return (UnOp Sum (Var (VarName "xs@1")))
                ]
            ]
    let typed =
          Program
            [ FunDef
                (FunName "solve@0")
                [ (VarName "xs@1", ATyList ATyInt)
                ]
                ATyInt
                [ Return (UnOp Sum (Var (VarName "xs@1")))
                ]
            ]
    run untyped `shouldBe` Right typed
  it "preserves type annotations" $ do
    let untyped =
          Program
            [ FunDef
                (FunName "solve@0")
                [ (VarName "n@1", ATyVar (TypeName "t@0"))
                ]
                (ATyVar (TypeName "t@1"))
                [ Define
                    (VarName "xs@2")
                    (ATyArray (ATyVar (TypeName "t@2")) (Var (VarName "n@1")))
                    ( ListComp
                        ( Comprehension
                            (ATyVar (TypeName "t@3"))
                            (Lit (LitBool False))
                            (VarName "x@3")
                            (ATyVar (TypeName "t@4"))
                            (UnOp Range1 (Var (VarName "n@1")))
                            Nothing
                        )
                    ),
                  Return (Var (VarName "xs@2"))
                ]
            ]
    let typed =
          Program
            [ FunDef
                (FunName "solve@0")
                [ (VarName "n@1", ATyNat)
                ]
                (ATyArray ATyBool (Var (VarName "n@1")))
                [ Define
                    (VarName "xs@2")
                    (ATyArray ATyBool (Var (VarName "n@1")))
                    ( ListComp
                        ( Comprehension
                            ATyBool
                            (Lit (LitBool False))
                            (VarName "x@3")
                            ATyNat
                            (UnOp Range1 (Var (VarName "n@1")))
                            Nothing
                        )
                    ),
                  Return (Var (VarName "xs@2"))
                ]
            ]
    run untyped `shouldBe` Right typed
  it "doesn't leak variables in annotational types" $ do
    let untyped =
          Program
            [ FunDef
                (FunName "solve@0")
                []
                (ATyVar (TypeName "t@0"))
                [ Define (VarName "n@1") (ATyVar (TypeName "t@1")) (Lit (LitInt 100)),
                  Define
                    (VarName "xs@2")
                    (ATyArray (ATyVar (TypeName "t@2")) (Var (VarName "n@1")))
                    ( ListComp
                        ( Comprehension
                            (ATyVar (TypeName "t@3"))
                            (Lit (LitBool False))
                            (VarName "x@3")
                            (ATyVar (TypeName "t@4"))
                            (UnOp Range1 (Var (VarName "n@1")))
                            Nothing
                        )
                    ),
                  Return (Var (VarName "xs@2"))
                ]
            ]
    let typed =
          Program
            [ FunDef
                (FunName "solve@0")
                []
                (ATyList ATyBool)
                [ Define (VarName "n@1") ATyNat (Lit (LitInt 100)),
                  Define
                    (VarName "xs@2")
                    (ATyArray ATyBool (Var (VarName "n@1")))
                    ( ListComp
                        ( Comprehension
                            ATyBool
                            (Lit (LitBool False))
                            (VarName "x@3")
                            ATyNat
                            (UnOp Range1 (Var (VarName "n@1")))
                            Nothing
                        )
                    ),
                  Return (Var (VarName "xs@2"))
                ]
            ]
    run untyped `shouldBe` Right typed
  it "converts types in operators" $ do
    let untyped =
          Program
            [ FunDef
                (FunName "solve@0")
                [(VarName "xs@1", ATyVar (TypeName "t@0"))]
                (ATyList ATyNat)
                [ Return (UnOp (Sorted (TyVar (TypeName "t@1"))) (Var (VarName "xs@1")))
                ]
            ]
    let typed =
          Program
            [ FunDef
                (FunName "solve@0")
                [(VarName "xs@1", ATyList ATyNat)]
                (ATyList ATyNat)
                [ Return (UnOp (Sorted TyInt) (Var (VarName "xs@1")))
                ]
            ]
    run untyped `shouldBe` Right typed
