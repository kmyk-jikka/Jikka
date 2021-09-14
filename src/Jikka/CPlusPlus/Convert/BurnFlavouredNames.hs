{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Jikka.CPlusPlus.Convert.BurnFlavouredNames
-- Description : remove unique numbers from names as a preprocess to emit the result source code. / 結果のソースコードを出力する前処理として、名前に付けられた一意な整数を解決します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.CPlusPlus.Convert.BurnFlavouredNames
  ( run,
  )
where

import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Jikka.CPlusPlus.Language.Expr
import Jikka.CPlusPlus.Language.Util
import Jikka.Common.Alpha
import Jikka.Common.Error

data Env = Env
  { renameMapping :: M.Map VarName VarName,
    usedVars :: S.Set String
  }
  deriving (Eq, Ord, Read, Show)

emptyEnv :: Env
emptyEnv =
  Env
    { renameMapping = M.empty,
      usedVars = S.empty
    }

fromNameKind :: Maybe NameKind -> String
fromNameKind = \case
  Nothing -> "u"
  Just LocalNameKind -> "x"
  Just LocalArgumentNameKind -> "b"
  Just LoopCounterNameKind -> "i"
  Just ConstantNameKind -> "c"
  Just FunctionNameKind -> "f"
  Just ArgumentNameKind -> "a"

chooseOccName :: S.Set String -> VarName -> String
chooseOccName used (VarName occ _ kind) =
  let occ_workaround = (\s -> if '$' `elem` s then Nothing else Just s) =<< occ -- TODO: Remove this after Python stops using variables with `$`.
      base = fromMaybe (fromNameKind kind) occ_workaround
      occs = base : map (\i -> base ++ show i) [2 ..]
      occ' = head $ filter (`S.notMember` used) occs
   in occ'

rename :: MonadState Env m => VarName -> m VarName
rename x = do
  y <- gets $ M.lookup x . renameMapping
  case y of
    Just y -> return y
    Nothing -> do
      y' <- flip chooseOccName x <$> gets usedVars
      let y = VarName (Just y') Nothing Nothing
      modify $ \env ->
        env
          { renameMapping = M.insert x y (renameMapping env),
            usedVars = S.insert y' (usedVars env)
          }
      return y

mapVarNameExprStatementGenericM :: forall m a. Monad m => ((Expr -> m Expr) -> (Statement -> m [Statement]) -> a) -> (VarName -> m VarName) -> a
mapVarNameExprStatementGenericM mapExprStatementM f = mapExprStatementM goE (fmap (: []) . goS)
  where
    goE :: Monad m => Expr -> m Expr
    goE = \case
      Var x -> Var <$> f x
      Lam args ret body -> Lam <$> mapM (\(t, x) -> (t,) <$> f x) args <*> pure ret <*> pure body
      e -> return e
    goLeftExpr :: Monad m => LeftExpr -> m LeftExpr
    goLeftExpr = \case
      LeftVar x -> LeftVar <$> f x
      LeftAt e1 e2 -> LeftAt <$> goLeftExpr e1 <*> pure e2
      LeftGet n e -> LeftGet n <$> goLeftExpr e
    goAssignExpr :: Monad m => AssignExpr -> m AssignExpr
    goAssignExpr = \case
      AssignExpr op e1 e2 -> AssignExpr op <$> goLeftExpr e1 <*> pure e2
      AssignIncr e -> AssignIncr <$> goLeftExpr e
      AssignDecr e -> AssignDecr <$> goLeftExpr e
    goS :: Monad m => Statement -> m Statement
    goS = \case
      For t x init pred incr body -> For t <$> f x <*> pure init <*> pure pred <*> goAssignExpr incr <*> pure body
      ForEach t x e body -> ForEach t <$> f x <*> pure e <*> pure body
      Declare t x init -> Declare t <$> f x <*> pure init
      DeclareDestructure xs e -> DeclareDestructure <$> mapM f xs <*> pure e
      Assign e -> Assign <$> goAssignExpr e
      stmt -> return stmt

mapVarNameToplevelStatementM :: Monad m => (VarName -> m VarName) -> ToplevelStatement -> m ToplevelStatement
mapVarNameToplevelStatementM f stmt = do
  stmt <- case stmt of
    VarDef t x e -> VarDef t <$> f x <*> pure e
    FunDef ret g args body -> FunDef ret g <$> mapM (\(t, x) -> (t,) <$> f x) args <*> pure body
    _ -> return stmt
  mapVarNameExprStatementGenericM mapExprStatementToplevelStatementM f stmt

mapVarNameProgramM :: Monad m => (VarName -> m VarName) -> Program -> m Program
mapVarNameProgramM f = mapToplevelStatementProgramM (fmap (: []) . mapVarNameToplevelStatementM f)

runProgram :: MonadState Env m => Program -> m Program
runProgram = mapVarNameProgramM rename

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.CPlusPlus.Convert.BurnFlavouredNames" $ do
  evalStateT (runProgram prog) emptyEnv
