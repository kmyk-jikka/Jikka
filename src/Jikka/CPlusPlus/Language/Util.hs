{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Jikka.CPlusPlus.Language.Util where

import Control.Monad.Identity
import qualified Data.Set as S
import Jikka.CPlusPlus.Language.Expr
import Jikka.Common.Alpha

fromLeftExpr :: LeftExpr -> Expr
fromLeftExpr = \case
  LeftVar x -> Var x
  LeftAt x e -> Call' At [fromLeftExpr x, e]
  LeftGet n e -> Call' (Function "std::get" [TyIntValue n]) [fromLeftExpr e]

newFreshName :: MonadAlpha m => NameKind -> m VarName
newFreshName kind = do
  i <- nextCounter
  return (VarName Nothing (Just i) (Just kind))

renameVarName :: MonadAlpha m => NameKind -> VarName -> m VarName
renameVarName kind (VarName occ _ _) = case occ of
  Nothing -> newFreshName kind
  Just occ -> renameVarName' kind occ

renameVarName' :: MonadAlpha m => NameKind -> String -> m VarName
renameVarName' kind occ = do
  i <- nextCounter
  return (VarName (Just occ) (Just i) (Just kind))

freeVars :: Expr -> S.Set VarName
freeVars = \case
  Var x -> S.singleton x
  Lit _ -> S.empty
  UnOp _ e -> freeVars e
  BinOp _ e1 e2 -> freeVars e1 <> freeVars e2
  Cond e1 e2 e3 -> freeVars e1 <> freeVars e2 <> freeVars e3
  Lam args _ body -> freeVarsStatements body S.\\ S.fromList (map snd args)
  Call f args -> freeVars f <> mconcat (map freeVars args)
  Callable _ -> S.empty

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
  Declare _ x init -> S.singleton x <> freeVarsDeclareRight init
  DeclareDestructure xs e -> S.fromList xs <> freeVars e
  Assign e -> freeVarsAssignExpr e
  Assert e -> freeVars e
  Return e -> freeVars e

freeVarsDeclareRight :: DeclareRight -> S.Set VarName
freeVarsDeclareRight = \case
  DeclareDefault -> S.empty
  DeclareCopy e -> freeVars e
  DeclareInitialize es -> S.unions (map freeVars es)

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

pattern Call' :: Function -> [Expr] -> Expr
pattern Call' f args = Call (Callable f) args

size :: Expr -> Expr
size e = Call' MethodSize [e]

at :: Expr -> Expr -> Expr
at e i = Call' At [e, i]

cast :: Type -> Expr -> Expr
cast t e = Call' (Cast t) [e]

assignSimple :: VarName -> Expr -> Statement
assignSimple x e = Assign (AssignExpr SimpleAssign (LeftVar x) e)

assignAt :: VarName -> Expr -> Expr -> Statement
assignAt xs i e = Assign (AssignExpr SimpleAssign (LeftAt (LeftVar xs) i) e)

callFunction :: FunName -> [Type] -> [Expr] -> Expr
callFunction f ts args = Call' (Function f ts) args

callFunction' :: FunName -> [Type] -> [Expr] -> Statement
callFunction' = ((ExprStatement .) .) . callFunction

callMethod :: Expr -> FunName -> [Expr] -> Expr
callMethod e f args = Call' (Method f) (e : args)

callMethod' :: Expr -> FunName -> [Expr] -> Statement
callMethod' = ((ExprStatement .) .) . callMethod

vecCtor :: Type -> [Expr] -> Expr
vecCtor t es = Call' (VecCtor t) es

begin :: Expr -> Expr
begin e = Call' (Method "begin") [e]

end :: Expr -> Expr
end e = Call' (Method "end") [e]

mapExprStatementExprM :: Monad m => (Expr -> m Expr) -> (Statement -> m [Statement]) -> Expr -> m Expr
mapExprStatementExprM f g = go
  where
    go = \case
      Var x -> f (Var x)
      Lit lit -> f (Lit lit)
      UnOp op e -> f . UnOp op =<< go e
      BinOp op e1 e2 -> f =<< (BinOp op <$> go e1 <*> go e2)
      Cond e1 e2 e3 -> f =<< (Cond <$> go e1 <*> go e2 <*> go e3)
      Lam args ret body -> f . Lam args ret . concat =<< mapM (mapExprStatementStatementM f g) body
      Call g args -> f =<< (Call <$> go g <*> mapM go args)
      Callable g -> f $ Callable g

mapExprStatementLeftExprM :: Monad m => (Expr -> m Expr) -> (Statement -> m [Statement]) -> LeftExpr -> m LeftExpr
mapExprStatementLeftExprM f g = \case
  LeftVar x -> return $ LeftVar x
  LeftAt e1 e2 -> LeftAt <$> mapExprStatementLeftExprM f g e1 <*> mapExprStatementExprM f g e2
  LeftGet n e -> LeftGet n <$> mapExprStatementLeftExprM f g e

mapExprStatementAssignExprM :: Monad m => (Expr -> m Expr) -> (Statement -> m [Statement]) -> AssignExpr -> m AssignExpr
mapExprStatementAssignExprM f g = \case
  AssignExpr op e1 e2 -> AssignExpr op <$> mapExprStatementLeftExprM f g e1 <*> mapExprStatementExprM f g e2
  AssignIncr e -> AssignIncr <$> mapExprStatementLeftExprM f g e
  AssignDecr e -> AssignDecr <$> mapExprStatementLeftExprM f g e

mapExprStatementStatementM :: Monad m => (Expr -> m Expr) -> (Statement -> m [Statement]) -> Statement -> m [Statement]
mapExprStatementStatementM f g = go
  where
    go' e = mapExprStatementExprM f g e
    go'' body = concat <$> mapM go body
    go = \case
      ExprStatement e -> g . ExprStatement =<< go' e
      Block stmts -> g . Block =<< go'' stmts
      If e body1 body2 -> g =<< (If <$> go' e <*> go'' body1 <*> traverse go'' body2)
      For t x init pred incr body -> g =<< (For t x <$> go' init <*> go' pred <*> mapExprStatementAssignExprM f g incr <*> go'' body)
      ForEach t x e body -> g =<< (ForEach t x <$> go' e <*> go'' body)
      While e body -> g =<< (While <$> go' e <*> go'' body)
      Declare t x init -> do
        init <- case init of
          DeclareDefault -> return DeclareDefault
          DeclareCopy e -> DeclareCopy <$> go' e
          DeclareInitialize es -> DeclareInitialize <$> mapM go' es
        g $ Declare t x init
      DeclareDestructure xs e -> g . DeclareDestructure xs =<< go' e
      Assign e -> g . Assign =<< mapExprStatementAssignExprM f g e
      Assert e -> g . Assert =<< go' e
      Return e -> g . Return =<< go' e

mapExprStatementToplevelStatementM :: Monad m => (Expr -> m Expr) -> (Statement -> m [Statement]) -> ToplevelStatement -> m ToplevelStatement
mapExprStatementToplevelStatementM f g = \case
  VarDef t x e -> VarDef t x <$> mapExprStatementExprM f g e
  FunDef ret h args body -> FunDef ret h args <$> (concat <$> mapM (mapExprStatementStatementM f g) body)
  StaticAssert e msg -> StaticAssert <$> mapExprStatementExprM f g e <*> pure msg

mapExprStatementProgramM :: Monad m => (Expr -> m Expr) -> (Statement -> m [Statement]) -> Program -> m Program
mapExprStatementProgramM f g (Program decls) = Program <$> mapM (mapExprStatementToplevelStatementM f g) decls

mapExprStatementProgram :: (Expr -> Expr) -> (Statement -> [Statement]) -> Program -> Program
mapExprStatementProgram f g = runIdentity . mapExprStatementProgramM (return . f) (return . g)

mapSubExprM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
mapSubExprM f e = mapExprStatementExprM f (return . (: [])) e

-- | `mapDirectExprStatementM` replaces exprs which are direct children of a given statement.
mapDirectExprStatementM :: Monad m => (Expr -> m Expr) -> Statement -> m Statement
mapDirectExprStatementM f = \case
  ExprStatement e -> ExprStatement <$> f e
  Block stmts -> return $ Block stmts
  If e body1 body2 -> If <$> f e <*> pure body1 <*> pure body2
  For t x init pred incr body -> For t x <$> f init <*> f pred <*> mapDirectExprAssignExprM f incr <*> pure body
  ForEach t x e body -> ForEach t x <$> f e <*> pure body
  While e body -> While <$> f e <*> pure body
  Declare t x init ->
    Declare t x <$> case init of
      DeclareDefault -> return DeclareDefault
      DeclareCopy e -> DeclareCopy <$> f e
      DeclareInitialize es -> DeclareInitialize <$> mapM f es
  DeclareDestructure xs e -> DeclareDestructure xs <$> f e
  Assign e -> Assign <$> mapDirectExprAssignExprM f e
  Assert e -> Assert <$> f e
  Return e -> Return <$> f e

mapDirectExprAssignExprM :: Monad m => (Expr -> m Expr) -> AssignExpr -> m AssignExpr
mapDirectExprAssignExprM f = \case
  AssignExpr op e1 e2 -> AssignExpr op <$> mapDirectExprLeftExprM f e1 <*> f e2
  AssignIncr e -> AssignIncr <$> mapDirectExprLeftExprM f e
  AssignDecr e -> AssignDecr <$> mapDirectExprLeftExprM f e

mapDirectExprLeftExprM :: Monad m => (Expr -> m Expr) -> LeftExpr -> m LeftExpr
mapDirectExprLeftExprM f = \case
  LeftVar x -> pure $ LeftVar x
  LeftAt e1 e2 -> LeftAt <$> mapDirectExprLeftExprM f e1 <*> f e2
  LeftGet i e -> LeftGet i <$> mapDirectExprLeftExprM f e

replaceExpr :: VarName -> Expr -> Expr -> Expr
replaceExpr x e = runIdentity . mapExprStatementExprM go (return . (: []))
  where
    go = \case
      Var y | y == x -> return e
      e' -> return e'

replaceStatement :: VarName -> Expr -> Statement -> Statement
replaceStatement x e = head . runIdentity . mapExprStatementStatementM go (return . (: []))
  where
    go = \case
      Var y | y == x -> return e
      e' -> return e'

mapToplevelStatementProgramM :: Monad m => (ToplevelStatement -> m [ToplevelStatement]) -> Program -> m Program
mapToplevelStatementProgramM f prog = Program . concat <$> mapM f (decls prog)
