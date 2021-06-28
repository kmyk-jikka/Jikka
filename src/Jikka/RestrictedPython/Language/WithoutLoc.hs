module Jikka.RestrictedPython.Language.WithoutLoc where

import Jikka.Common.Location
import Jikka.RestrictedPython.Language.Expr

constIntExp :: Integer -> Expr'
constIntExp = withoutLoc . Constant . ConstInt

constBoolExp :: Bool -> Expr'
constBoolExp = withoutLoc . Constant . ConstBool

constBuiltinExp :: Builtin -> Expr'
constBuiltinExp = withoutLoc . Constant . ConstBuiltin

binOp :: Expr' -> Operator -> Expr' -> Expr'
binOp e1 op e2 = withoutLoc (BinOp e1 op e2)

addExp :: Expr' -> Expr' -> Expr'
addExp e1 e2 = binOp e1 Add e2

subExp :: Expr' -> Expr' -> Expr'
subExp e1 e2 = binOp e1 Sub e2

multExp :: Expr' -> Expr' -> Expr'
multExp e1 e2 = binOp e1 Mult e2

unaryOp :: UnaryOp -> Expr' -> Expr'
unaryOp op e = withoutLoc (UnaryOp op e)

eqExp :: Type -> Expr' -> Expr' -> Expr'
eqExp t e1 e2 = withoutLoc (Compare e1 (CmpOp' Eq' t) e2)

name :: VarName' -> Expr'
name = withoutLoc . Name

call :: Expr' -> [Expr'] -> Expr'
call f args = withoutLoc (Call f args)

list :: Type -> [Expr'] -> Expr'
list = (withoutLoc .) . List

listComp :: Expr' -> Comprehension -> Expr'
listComp = (withoutLoc .) . ListComp

subscript :: Expr' -> Expr' -> Expr'
subscript = (withoutLoc .) . Subscript

nameTrg :: VarName' -> Target'
nameTrg = withoutLoc . NameTrg

subscriptTrg :: Target' -> Expr' -> Target'
subscriptTrg = (withoutLoc .) . SubscriptTrg

tupleTrg :: [Target'] -> Target'
tupleTrg = withoutLoc . TupleTrg
