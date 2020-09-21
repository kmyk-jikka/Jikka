module Jikka.Language.Core.LintSpec
  ( spec,
  )
where

import qualified Data.Map as M
import Jikka.Language.Core.Lint
import Jikka.Language.Core.Type
import Test.Hspec

spec :: Spec
spec =
  specValidateExpr

specValidateExpr :: Spec
specValidateExpr = describe "validateExpr" $ do
  it "simple success" $ do
    let x = MkVar "x" intType
    let plus = MkVar "+" (FunTy intType (FunTy intType intType))
    let e1 = App (App (Var plus) (Var x)) (Var x)
    let e2 = Lit (Int 3)
    let expr = App (Lam x e1) e2
    let vars = [plus]
    let env = M.fromList (map (\x -> (varName x, x)) vars)
    validateExpr env expr `shouldBe` Right intType
  it "simple failure" $ do
    let expr = App (Lit (Int 3)) (Lit (Int 3))
    let vars = []
    let env = M.fromList (map (\x -> (varName x, x)) vars)
    validateExpr env expr `shouldBe` Left "type mismatch: cannot apply TyConApp IntTyCon [] to TyConApp IntTyCon []"
