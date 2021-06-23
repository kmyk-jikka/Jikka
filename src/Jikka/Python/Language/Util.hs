module Jikka.Python.Language.Util where

import Jikka.Python.Language.Expr

constIntExp :: Integer -> Expr
constIntExp = Constant . ConstInt

constBoolExp :: Bool -> Expr
constBoolExp = Constant . ConstBool
