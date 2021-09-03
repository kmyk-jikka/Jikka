{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Jikka.Core.Language.QuasiRules
  ( r,

    -- * Things which `r` uses.
    module Jikka.Core.Language.Expr,
    alphaExpr,
    makeRewriteRule,
    genVarName',
  )
where

import Control.Arrow
import Control.Monad.State.Strict
import Data.Data
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.Format.Error
import qualified Jikka.Core.Convert.Alpha as Alpha
import qualified Jikka.Core.Convert.TypeInfer as TypeInfer
import Jikka.Core.Language.Expr
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util
import Jikka.Core.Parse (parseRule)
import Language.Haskell.TH (Exp (..), Lit (..), Pat (..), Q, Stmt (..))
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH

liftError :: ExceptT Error Q a -> Q a
liftError f = do
  x <- runExceptT f
  case x of
    Left err -> fail $ "Jikka.Core.Language.QuasiRules.liftError: " ++ unlines (prettyError' err)
    Right y -> return y

fromVarName :: VarName -> Q TH.Name
fromVarName (VarName x) = do
  let base = takeWhile (/= '$') x
  TH.newName (if null base then "x" else base)

fromTypeName :: TypeName -> Q TH.Name
fromTypeName (TypeName x) = do
  let base = takeWhile (/= '$') x
  TH.newName (if null base then "t" else base)

liftDataP :: Data a => a -> Q Pat
liftDataP = TH.dataToPatQ (const Nothing)

-- | `Exp` with type `Expr`.
type ExprExp = Exp

data RenamedVarName
  = DeclaredAtForall
  | RenamedVar ExprExp
  | RenamedPatLam TH.Name ExprExp
  | RenamedExpLam ExprExp
  | RenamedPatLet TH.Name ExprExp
  | RenamedExpLet ExprExp
  deriving (Eq, Ord, Show)

expFromRenamedVarName :: RenamedVarName -> Maybe ExprExp
expFromRenamedVarName = \case
  DeclaredAtForall -> Nothing
  RenamedVar e -> Just e
  RenamedPatLam _ e -> Just e
  RenamedExpLam e -> Just e
  RenamedPatLet _ e -> Just e
  RenamedExpLet e -> Just e

data Env = Env
  { vars :: [(VarName, RenamedVarName)],
    typeVars :: [(TypeName, TH.Name)]
  }

toPatT :: Type -> StateT Env Q Pat
toPatT = \case
  VarTy x -> do
    env <- gets typeVars
    case lookup x env of
      Just y -> do
        lift [p|((==) $(pure (VarE y)) -> True)|]
      Nothing -> do
        y <- lift $ fromTypeName x
        modify' (\env -> env {typeVars = (x, y) : typeVars env})
        return $ VarP y
  IntTy -> lift $ liftDataP IntTy
  BoolTy -> lift $ liftDataP IntTy
  ListTy t -> do
    t <- toPatT t
    lift [p|ListTy $(pure t)|]
  TupleTy ts -> do
    ts <- mapM toPatT ts
    lift [p|TupleTy $(pure (ListP ts))|]
  FunTy t1 t2 -> do
    t1 <- toPatT t1
    t2 <- toPatT t2
    lift [p|FunTy $(pure t1) $(pure t2)|]
  DataStructureTy ds -> do
    lift [p|DataStructureTy $(liftDataP ds)|]

toPatL :: Literal -> StateT Env Q Pat
toPatL = \case
  LitBuiltin builtin ts -> do
    ts <- mapM toPatT ts
    lift [p|LitBuiltin $(liftDataP builtin) $(pure (ListP ts))|]
  lit@(LitInt _) -> lift $ liftDataP lit
  lit@(LitBool _) -> lift $ liftDataP lit
  LitNil t -> do
    t <- toPatT t
    lift [p|LitNil $(pure t)|]
  LitBottom t msg -> do
    t <- toPatT t
    lift [p|LitBottom $(pure t) $(liftDataP msg)|]

toPatE :: Expr -> StateT Env Q Pat
toPatE = \case
  Var x ->
    if x == VarName "_"
      then return WildP
      else do
        env <- gets vars
        case expFromRenamedVarName <$> lookup x env of
          Just (Just y) -> do
            lift [p|((== $(pure y)) -> True)|]
          Just Nothing -> do
            y <- lift $ fromVarName x
            modify' (\env -> env {vars = (x, RenamedVar (VarE y)) : vars env})
            return $ VarP y
          Nothing -> fail $ "Jikka.Core.Language.QuasiRules.toPatE: undefined variable (forall is required): " ++ unVarName x
  Lit lit -> do
    lit <- toPatL lit
    lift [p|Lit $(pure lit)|]
  App e1 e2 -> do
    e1 <- toPatE e1
    e2 <- toPatE e2
    lift [p|App $(pure e1) $(pure e2)|]
  Lam x t e -> do
    t <- toPatT t
    y <- lift $ fromVarName x
    y' <- lift [e|Var $(pure (VarE y))|]
    modify' (\env -> env {vars = (x, RenamedPatLam y y') : vars env})
    e <- toPatE e
    lift [p|Lam $(pure (VarP y)) $(pure t) $(pure e)|]
  Let x t e1 e2 -> do
    t <- toPatT t
    e1 <- toPatE e1
    y <- lift $ fromVarName x
    y' <- lift [e|Var $(pure (VarE y))|]
    modify' (\env -> env {vars = (x, RenamedPatLet y y') : vars env})
    e2 <- toPatE e2
    lift [p|Let $(pure (VarP y)) $(pure t) $(pure e1) $(pure e2)|]
  Assert e1 e2 -> do
    e1 <- toPatE e1
    e2 <- toPatE e2
    lift [p|Assert $(pure e1) $(pure e2)|]

toExpT :: Type -> StateT Env Q Exp
toExpT = \case
  VarTy x -> do
    env <- gets typeVars
    case lookup x env of
      Just y -> return $ VarE y
      Nothing -> fail $ "Jikka.Core.Language.QuasiRules.toExpT: undefined type variable: " ++ unTypeName x
  IntTy -> do
    lift $ TH.liftData IntTy
  BoolTy -> do
    lift $ TH.liftData BoolTy
  ListTy t -> do
    t <- toExpT t
    lift [e|ListTy $(pure t)|]
  TupleTy ts -> do
    ts <- mapM toExpT ts
    lift [e|TupleTy $(pure (ListE ts))|]
  FunTy t1 t2 -> do
    t1 <- toExpT t1
    t2 <- toExpT t2
    lift [e|FunTy $(pure t1) $(pure t2)|]
  DataStructureTy ds -> do
    lift $ TH.liftData (DataStructureTy ds)

toExpL :: Literal -> StateT Env Q Exp
toExpL = \case
  LitBuiltin builtin ts -> do
    ts <- mapM toExpT ts
    lift [e|LitBuiltin $(TH.liftData builtin) $(pure (ListE ts))|]
  lit@(LitInt _) -> lift $ TH.liftData lit
  lit@(LitBool _) -> lift $ TH.liftData lit
  LitNil t -> do
    t <- toExpT t
    lift [e|LitNil $(pure t)|]
  LitBottom t msg -> do
    t <- toExpT t
    lift [e|LitBottom $(pure t) $(TH.liftData msg)|]

toExpE :: Expr -> StateT Env Q ([Stmt], Exp)
toExpE e = do
  var <- lift [e|Var|]
  genVarName <- lift [e|genVarName'|]
  case e of
    Var x -> do
      env <- gets vars
      case expFromRenamedVarName <$> lookup x env of
        Just (Just y) -> return ([], y)
        _ -> fail $ "Jikka.Core.Language.QuasiRules.toExpE: undefined variable: " ++ unVarName x
    Lit lit -> do
      lit <- toExpL lit
      e <- lift [e|Lit $(pure lit)|]
      return ([], e)
    App e1 e2 -> do
      (stmts, e1) <- toExpE e1
      (stmts', e2) <- toExpE e2
      e <- lift [e|App $(pure e1) $(pure e2)|]
      return (stmts ++ stmts', e)
    Lam x t e -> do
      t <- toExpT t
      y <- gets (lookup x . vars)
      case y of
        Just (RenamedPatLam y _) -> do
          -- Use the same variable name
          (stmts, e) <- toExpE e
          e <- lift [e|Lam $(pure (VarE y)) $(pure t) $(pure e)|]
          return (stmts, e)
        Nothing -> do
          -- Introduce a new name
          y <- lift $ fromVarName x
          modify' (\env -> env {vars = (x, RenamedExpLam (AppE var (VarE y))) : vars env})
          (stmts, e) <- toExpE e
          e <- lift [e|Lam $(pure (VarE y)) $(pure t) $(pure e)|]
          return (BindS (VarP y) genVarName : stmts, e)
        _ -> fail $ "Jikka.Core.Language.QuasiRules.toExpE: variable conflicts: " ++ unVarName x
    Let x t e1 e2 -> do
      t <- toExpT t
      (stmts, e1) <- toExpE e1
      y <- gets (lookup x . vars)
      case y of
        Just (RenamedPatLet y _) -> do
          -- Use the same variable name
          (stmts', e2) <- toExpE e2
          e <- lift [e|Let $(pure (VarE y)) $(pure t) $(pure e1) $(pure e2)|]
          return (stmts ++ stmts', e)
        Nothing -> do
          -- Introduce a new name
          y <- lift $ fromVarName x
          modify' (\env -> env {vars = (x, RenamedExpLet (AppE var (VarE y))) : vars env})
          (stmts', e2) <- toExpE e2
          e <- lift [e|Let $(pure (VarE y)) $(pure t) $(pure e1) $(pure e2)|]
          return (stmts ++ BindS (VarP y) genVarName : stmts', e)
        _ -> fail $ "Jikka.Core.Language.QuasiRules.toExpE: variable conflicts: " ++ unVarName x
    Assert e1 e2 -> do
      (stmts1, e1) <- toExpE e1
      (stmts2, e2) <- toExpE e2
      e <- lift [e|Assert $(pure e1) $(pure e2)|]
      return (stmts1 ++ stmts2, e)

alphaExpr :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> m Expr
alphaExpr = Alpha.runExpr

ruleExp :: String -> Q Exp
ruleExp s = do
  (name, args, e1, e2) <- liftError $ parseRule s
  (args, e1, e2) <- liftError $ TypeInfer.runRule args e1 e2
  env <-
    return $
      Env
        { vars = map (second (const DeclaredAtForall)) args,
          typeVars = []
        }
  (pat, env) <- runStateT (toPatE e1) env
  supressUnusedMatchesWarnings <- (concat <$>) . forM (vars env) $ \case
    (_, expFromRenamedVarName -> Just e) -> do
      e <- [e|return $(pure e)|]
      return [NoBindS e]
    _ -> return []
  supressUnusedMatchesWarnings' <- forM (typeVars env) $ \(_, y) -> do
    NoBindS <$> [e|return $(pure (VarE y))|]
  ((stmts, exp), _) <- runStateT (toExpE e2) env
  nop <- [e|return ()|]
  exp' <- [e|return $(pure exp)|]
  [e|
    makeRewriteRule $(pure (LitE (StringL name))) $ \env e -> case e of
      $(pure pat) -> do
        $(pure (DoE (supressUnusedMatchesWarnings ++ [NoBindS nop])))
        $(pure (DoE (supressUnusedMatchesWarnings' ++ [NoBindS nop])))
        e <- $(pure (DoE (stmts ++ [NoBindS exp'])))
        Just <$> alphaExpr (typeEnv env) e
      _ -> return Nothing
    |]

r :: TH.QuasiQuoter
r =
  TH.QuasiQuoter
    { TH.quoteExp = ruleExp,
      TH.quotePat = undefined,
      TH.quoteType = undefined,
      TH.quoteDec = undefined
    }
