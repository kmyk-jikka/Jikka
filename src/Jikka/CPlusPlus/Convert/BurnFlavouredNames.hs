{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

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
import Jikka.Common.Name

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

fromNameHint :: Maybe NameHint -> String
fromNameHint = \case
  Nothing -> "u"
  Just LocalNameHint -> "x"
  Just LocalArgumentNameHint -> "b"
  Just LoopCounterNameHint -> "i"
  Just ConstantNameHint -> "c"
  Just FunctionNameHint -> "f"
  Just ArgumentNameHint -> "a"
  Just (AdHocNameHint hint) -> hint

chooseOccName :: S.Set String -> VarName -> String
chooseOccName used (VarName occ _ kind) =
  let occ_workaround = (\s -> if '$' `elem` s then Nothing else Just s) =<< occ -- TODO: Remove this after Python stops using variables with `$`.
      base = fromMaybe (fromNameHint kind) occ_workaround
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

runProgram :: MonadState Env m => Program -> m Program
runProgram = mapVarNameProgramM rename

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.CPlusPlus.Convert.BurnFlavouredNames" $ do
  evalStateT (runProgram prog) emptyEnv
