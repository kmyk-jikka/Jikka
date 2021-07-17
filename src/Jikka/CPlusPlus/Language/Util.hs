{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Language.Util where

import Control.Monad.Identity
import Data.Char (isAlphaNum)
import Jikka.CPlusPlus.Language.Expr
import Jikka.Common.Alpha

fromLeftExpr :: LeftExpr -> Expr
fromLeftExpr = \case
  LeftVar x -> Var x
  LeftAt x e -> At (fromLeftExpr x) e
  LeftGet n e -> Call (Function "std::get" [TyIntValue n]) [fromLeftExpr e]

data NameKind
  = LocalNameKind
  | LocalArgumentNameKind
  | LoopCounterNameKind
  | ConstantNameKind
  | FunctionNameKind
  | ArgumentNameKind
  deriving (Eq, Ord, Show, Read)

fromNameKind :: NameKind -> String
fromNameKind = \case
  LocalNameKind -> "x"
  LocalArgumentNameKind -> "b"
  LoopCounterNameKind -> "i"
  ConstantNameKind -> "c"
  FunctionNameKind -> "f"
  ArgumentNameKind -> "a"

newFreshName :: MonadAlpha m => NameKind -> m VarName
newFreshName kind = renameVarName kind ""

renameVarName :: MonadAlpha m => NameKind -> String -> m VarName
renameVarName kind hint = do
  i <- nextCounter
  let prefix = case takeWhile (\c -> isAlphaNum c || c == '_') hint of
        "" -> fromNameKind kind
        hint' -> hint' ++ "_"
  return (VarName (prefix ++ show i))

freeVars :: Expr -> [VarName]
freeVars = \case
  Var x -> [x]
  Lit _ -> []
  UnOp _ e -> freeVars e
  BinOp _ e1 e2 -> freeVars e1 ++ freeVars e2
  Cond e1 e2 e3 -> freeVars e1 ++ freeVars e2 ++ freeVars e3
  Lam _ _ _ -> error "Jikka.CPlusPlus.Language.Util.freeVars: TODO"
  Call _ _ -> error "Jikka.CPlusPlus.Language.Util.freeVars: TODO"
  ArrayExt _ es -> concatMap freeVars es
  VecExt _ es -> concatMap freeVars es
  At e1 e2 -> freeVars e1 ++ freeVars e2
  Cast _ e -> freeVars e

cinStatement :: Expr -> Statement
cinStatement e = ExprStatement (BinOp BitRightShift (Var "std::cin") e)

coutStatement :: Expr -> Statement
coutStatement e = ExprStatement (BinOp BitLeftShift (BinOp BitLeftShift (Var "std::cout") e) (Lit (LitChar ' ')))

repStatement :: VarName -> Expr -> [Statement] -> Statement
repStatement i n body = For TyInt32 i (Lit (LitInt32 0)) (BinOp LessThan (Var i) n) (AssignIncr (LeftVar i)) body

litInt64 :: Integer -> Expr
litInt64 n = Lit (LitInt64 n)

litInt32 :: Integer -> Expr
litInt32 n = Lit (LitInt32 n)

incrExpr :: Expr -> Expr
incrExpr e = BinOp Add e (Lit (LitInt32 1))

-- | `fastSize` calls @vector<T>::size()@ method with an optimization aroung `Jikka.Core.Language.Range1`.
fastSize :: Expr -> Expr
fastSize (Call (Function "jikka::range" []) [n]) = n
fastSize e = Call (Method e "size") []

-- | `fastAt` is subscription with an optimization aroung `Jikka.Core.Language.Range1`.
fastAt :: Expr -> Expr -> Expr
fastAt (Call (Function "jikka::range" []) [_]) i = i
fastAt e i = At e i

assignSimple :: VarName -> Expr -> Statement
assignSimple x e = Assign (AssignExpr SimpleAssign (LeftVar x) e)

assignAt :: VarName -> Expr -> Expr -> Statement
assignAt xs i e = Assign (AssignExpr SimpleAssign (LeftAt (LeftVar xs) i) e)

callExpr :: Expr -> [Expr] -> Expr
callExpr f args = Call (Callable f) args

callFunction :: FunName -> [Type] -> [Expr] -> Expr
callFunction f ts args = Call (Function f ts) args

callFunction' :: FunName -> [Type] -> [Expr] -> Statement
callFunction' = ((ExprStatement .) .) . callFunction

callMethod :: Expr -> FunName -> [Expr] -> Expr
callMethod e f args = Call (Method e f) args

callMethod' :: Expr -> FunName -> [Expr] -> Statement
callMethod' = ((ExprStatement .) .) . callMethod

begin :: Expr -> Expr
begin e = Call (Method e "begin") []

end :: Expr -> Expr
end e = Call (Method e "end") []

mapExprStatementExprM :: Monad m => (Expr -> m Expr) -> (Statement -> m Statement) -> Expr -> m Expr
mapExprStatementExprM f g = go
  where
    go = \case
      Var x -> f (Var x)
      Lit lit -> f (Lit lit)
      UnOp op e -> f . UnOp op =<< go e
      BinOp op e1 e2 -> f =<< (BinOp op <$> go e1 <*> go e2)
      Cond e1 e2 e3 -> f =<< (Cond <$> go e1 <*> go e2 <*> go e3)
      Lam args ret body -> f . Lam args ret =<< mapM (mapExprStatementStatementM f g) body
      Call e args -> f =<< (Call <$> mapExprStatementFunctionM f g e <*> mapM go args)
      ArrayExt t es -> f . ArrayExt t =<< mapM go es
      VecExt t es -> f . VecExt t =<< mapM go es
      At e1 e2 -> f =<< (At <$> go e1 <*> go e2)
      Cast t e -> f . Cast t =<< go e

mapExprStatementFunctionM :: Monad m => (Expr -> m Expr) -> (Statement -> m Statement) -> Function -> m Function
mapExprStatementFunctionM f g = \case
  Callable e -> Callable <$> mapExprStatementExprM f g e
  Function h ts -> return $ Function h ts
  Method e h -> Method <$> mapExprStatementExprM f g e <*> return h

mapExprStatementLeftExprM :: Monad m => (Expr -> m Expr) -> (Statement -> m Statement) -> LeftExpr -> m LeftExpr
mapExprStatementLeftExprM f g = \case
  LeftVar x -> return $ LeftVar x
  LeftAt e1 e2 -> LeftAt <$> mapExprStatementLeftExprM f g e1 <*> mapExprStatementExprM f g e2
  LeftGet n e -> LeftGet n <$> mapExprStatementLeftExprM f g e

mapExprStatementAssignExprM :: Monad m => (Expr -> m Expr) -> (Statement -> m Statement) -> AssignExpr -> m AssignExpr
mapExprStatementAssignExprM f g = \case
  AssignExpr op e1 e2 -> AssignExpr op <$> mapExprStatementLeftExprM f g e1 <*> mapExprStatementExprM f g e2
  AssignIncr e -> AssignIncr <$> mapExprStatementLeftExprM f g e
  AssignDecr e -> AssignDecr <$> mapExprStatementLeftExprM f g e

mapExprStatementStatementM :: Monad m => (Expr -> m Expr) -> (Statement -> m Statement) -> Statement -> m Statement
mapExprStatementStatementM f g = go
  where
    go' = mapExprStatementExprM f g
    go = \case
      ExprStatement e -> g . ExprStatement =<< go' e
      Block stmts -> g . Block =<< mapM go stmts
      If e body1 body2 -> g =<< (If <$> go' e <*> mapM go body1 <*> traverse (mapM go) body2)
      For t x init pred incr body -> g =<< (For t x <$> go' init <*> go' pred <*> mapExprStatementAssignExprM f g incr <*> mapM go body)
      ForEach t x e body -> g =<< (ForEach t x <$> go' e <*> mapM go body)
      While e body -> g =<< (While <$> go' e <*> mapM go body)
      Declare t x e -> g . Declare t x =<< traverse go' e
      DeclareDestructure xs e -> g . DeclareDestructure xs =<< go' e
      Assign e -> g . Assign =<< mapExprStatementAssignExprM f g e
      Assert e -> g . Assert =<< go' e
      Return e -> g . Return =<< go' e

replaceExpr :: VarName -> Expr -> Expr -> Expr
replaceExpr x e = runIdentity . mapExprStatementExprM go return
  where
    go = \case
      Var y | y == x -> return e
      e' -> return e'

replaceStatement :: VarName -> Expr -> Statement -> Statement
replaceStatement x e = runIdentity . mapExprStatementStatementM go return
  where
    go = \case
      Var y | y == x -> return e
      e' -> return e'
