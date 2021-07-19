{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Jikka.CPlusPlus.Language.Util where

import Control.Monad.Identity
import Data.Char (isAlphaNum)
import qualified Data.Set as S
import Jikka.CPlusPlus.Language.Expr
import Jikka.Common.Alpha

fromLeftExpr :: LeftExpr -> Expr
fromLeftExpr = \case
  LeftVar x -> Var x
  LeftAt x e -> Call At [fromLeftExpr x, e]
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

freeVars :: Expr -> S.Set VarName
freeVars = \case
  Var x -> S.singleton x
  Lit _ -> S.empty
  UnOp _ e -> freeVars e
  BinOp _ e1 e2 -> freeVars e1 <> freeVars e2
  Cond e1 e2 e3 -> freeVars e1 <> freeVars e2 <> freeVars e3
  Lam args _ body -> freeVarsStatements body S.\\ S.fromList (map snd args)
  Call _ args -> mconcat (map freeVars args)
  CallExpr f args -> freeVars f <> mconcat (map freeVars args)

freeVarsStatements :: [Statement] -> S.Set VarName
freeVarsStatements = mconcat . map freeVarsStatement

freeVarsStatement :: Statement -> S.Set VarName
freeVarsStatement = \case
  ExprStatement e -> freeVars e
  Block stmts -> freeVarsStatements stmts
  If e body1 body2 -> freeVars e <> freeVarsStatements body1 <> S.unions (fmap freeVarsStatements body2)
  For _ x init pred incr body -> S.singleton x <> freeVars init <> freeVars pred <> freeVarsAssignExpr incr <> freeVarsStatements body
  ForEach _ x e body -> S.singleton x <> freeVars e <> freeVarsStatements body
  While e body -> freeVars e <> freeVarsStatements body
  Declare _ x e -> S.singleton x <> S.unions (fmap freeVars e)
  DeclareDestructure xs e -> S.fromList xs <> freeVars e
  Assign e -> freeVarsAssignExpr e
  Assert e -> freeVars e
  Return e -> freeVars e

freeVarsAssignExpr :: AssignExpr -> S.Set VarName
freeVarsAssignExpr = \case
  AssignExpr _ e1 e2 -> freeVarsLeftExpr e1 <> freeVars e2
  AssignIncr e -> freeVarsLeftExpr e
  AssignDecr e -> freeVarsLeftExpr e

freeVarsLeftExpr :: LeftExpr -> S.Set VarName
freeVarsLeftExpr = \case
  LeftVar x -> S.singleton x
  LeftAt e1 e2 -> freeVarsLeftExpr e1 <> freeVars e2
  LeftGet _ e -> freeVarsLeftExpr e

shouldBeArray :: [Type] -> Bool
shouldBeArray ts = not (null ts) && ts == replicate (length ts) (head ts)

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
fastSize (Call Range [n]) = n
fastSize e = Call MethodSize [e]

-- | `fastAt` is subscription with an optimization aroung `Jikka.Core.Language.Range1`.
fastAt :: Expr -> Expr -> Expr
fastAt (Call Range [_]) i = i
fastAt e i = Call At [e, i]

cast :: Type -> Expr -> Expr
cast t e = Call (Cast t) [e]

assignSimple :: VarName -> Expr -> Statement
assignSimple x e = Assign (AssignExpr SimpleAssign (LeftVar x) e)

assignAt :: VarName -> Expr -> Expr -> Statement
assignAt xs i e = Assign (AssignExpr SimpleAssign (LeftAt (LeftVar xs) i) e)

callFunction :: FunName -> [Type] -> [Expr] -> Expr
callFunction f ts args = Call (Function f ts) args

callFunction' :: FunName -> [Type] -> [Expr] -> Statement
callFunction' = ((ExprStatement .) .) . callFunction

callMethod :: Expr -> FunName -> [Expr] -> Expr
callMethod e f args = Call (Method f) (e : args)

callMethod' :: Expr -> FunName -> [Expr] -> Statement
callMethod' = ((ExprStatement .) .) . callMethod

begin :: Expr -> Expr
begin e = Call (Method "begin") [e]

end :: Expr -> Expr
end e = Call (Method "end") [e]

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
      Call g args -> f . Call g =<< mapM go args
      CallExpr g args -> f =<< (CallExpr <$> go g <*> mapM go args)

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
