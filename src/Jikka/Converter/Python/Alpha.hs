module Jikka.Converter.Python.Alpha
  ( run,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Set as S
import Jikka.Language.Common.Name
import Jikka.Language.Common.Pos (value, withPos)
import Jikka.Language.Python.Parsed.Expr
import Jikka.Language.Python.Parsed.Stdlib (operatorNames)

--------------------------------------------------------------------------------
-- prepare a monad

data AlphaState
  = AlphaState
      { nextId :: Int,
        usedVars :: [(VarName, VarName)],
        usedFuns :: [(FunName, FunName)]
      }
  deriving (Eq, Ord, Show, Read)

type Alpha a = ExceptT String (State AlphaState) a

initialState :: AlphaState
initialState =
  AlphaState
    { nextId = 0,
      usedVars = [],
      usedFuns = map (\name -> (name, name)) (S.toList operatorNames)
    }

runAlpha :: Alpha a -> Either String a
runAlpha a = evalState (runExceptT a) initialState

--------------------------------------------------------------------------------
-- prepare utilities about names

genName :: Name -> Alpha Name
genName name = do
  id <- gets nextId
  modify $ \state -> state {nextId = id + 1}
  return $ name ++ "@" ++ show id

checkNotFunName :: VarName -> Alpha ()
checkNotFunName name = do
  name' <- lookup (FunName $ unVarName name) <$> gets usedFuns
  case name' of
    Nothing -> return ()
    Just _ -> throwError $ "the name of the variable is already used as a function: " ++ show (unVarName name)

checkNotVarName :: FunName -> Alpha ()
checkNotVarName name = do
  name' <- lookup (VarName $ unFunName name) <$> gets usedVars
  case name' of
    Nothing -> return ()
    Just _ -> throwError $ "the name of the function is already used as a variable: " ++ show (unFunName name)

-- TODO: add location info to the VarName
defineNewVarName :: VarName -> Alpha VarName
defineNewVarName name = do
  checkNotFunName name
  name' <- lookup name <$> gets usedVars
  case name' of
    Nothing -> do
      name' <- VarName <$> genName (unVarName name)
      modify $ \state -> state {usedVars = (name, name') : usedVars state}
      return name'
    _ -> throwError $ "variable is already defined: " ++ show (unVarName name)

-- TODO: add location info to the VarName
useExistingVarName :: VarName -> Alpha VarName
useExistingVarName name = do
  used <- gets usedVars
  case lookup name used of
    Nothing -> throwError $ "undefined variable is found: " ++ show (unVarName name)
    Just name' -> return name'

-- TODO: add location info to the FunName
defineNewFunName :: FunName -> Alpha FunName
defineNewFunName name = do
  checkNotVarName name
  name' <- lookup name <$> gets usedFuns
  case name' of
    Nothing -> do
      name' <- FunName <$> genName (unFunName name)
      modify $ \state -> state {usedFuns = (name, name') : usedFuns state}
      return name'
    Just _ -> throwError $ "function is already defined: " ++ show (unFunName name)

-- TODO: add location info to the FunName
useExistingFunName :: FunName -> Alpha FunName
useExistingFunName name = do
  used <- gets usedFuns
  case lookup name used of
    Nothing -> throwError $ "undefined function is found: " ++ show (unFunName name)
    Just name' -> return name'

-- withNewEnv runs the given action in a new separated sub-environment.
withNewEnv :: Alpha a -> Alpha a
withNewEnv cont = do
  usedVars' <- gets usedVars
  usedFuns' <- gets usedFuns
  result <- cont
  modify $ \state ->
    state
      { usedVars = usedVars',
        usedFuns = usedFuns'
      }
  return result

-- withEnvPreserving ensures that the given action doesn't changes the environment.
withEnvPreserving :: Alpha a -> Alpha a
withEnvPreserving cont = do
  usedVars' <- gets usedVars
  usedFuns' <- gets usedFuns
  result <- cont
  usedVars'' <- gets usedVars
  usedFuns'' <- gets usedFuns
  () <- if usedVars' == usedVars'' && usedFuns' == usedFuns'' then return () else error "env must be preserved, but actually not preserved"
  return result

delete :: Eq a => a -> [(a, b)] -> [(a, b)]
delete _ [] = []
delete a ((a', b) : cs) =
  if a == a'
    then delete a cs
    else (a', b) : delete a cs

-- withBreakingEnv runs the given action in a new separated sub-environment. After the action stops, it collects the newly-defined variables in the action, and removes them from the base environment.
withBreakingEnv :: Alpha a -> Alpha a
withBreakingEnv cont = do
  usedVars' <- gets usedVars
  usedFuns' <- gets usedFuns
  result <- cont
  usedVars'' <- gets usedVars
  usedFuns'' <- gets usedFuns
  () <- if usedFuns' == usedFuns'' then return () else error "env for functions must be preserved, but actually not preserved"
  let go :: [(VarName, VarName)] -> Alpha ()
      go names = case names of
        _ | names == usedVars' -> return ()
        [] -> error "the environment is broken"
        ((name, _) : names) -> do
          modify $ \state -> state {usedVars = delete name (usedVars state)}
          go names
  () <- go usedVars''
  return result

-- ---------------------------------------------------------------------------
-- convert AST

alphaType :: Type' -> Alpha Type'
alphaType t = withPos t <$> case value t of
  TyInt -> return TyInt
  TyNat -> return TyNat
  TyInterval l r -> TyInterval <$> alphaExpr l <*> alphaExpr r
  TyBool -> return TyBool
  TyList t' -> TyList <$> alphaType t'
  TyIterator t' -> TyIterator <$> alphaType t'
  TyArray t' n -> TyArray <$> alphaType t' <*> alphaExpr n

alphaMaybeType :: Maybe Type' -> Alpha (Maybe Type')
alphaMaybeType t = case t of
  Nothing -> return Nothing
  Just t -> Just <$> alphaType t

alphaComprehension :: Comprehension -> Alpha Comprehension
alphaComprehension (Comprehension e1 name e2 e3) = do
  e2 <- alphaExpr e2
  preservedVars <- gets usedVars
  name' <- case name of
    Nothing -> VarName <$> genName "_"
    Just name -> do
      checkNotFunName name
      name' <- VarName <$> genName (unVarName name)
      modify $ \state -> state {usedVars = (name, name') : preservedVars}
      return name'
  e3 <- withEnvPreserving $ mapM alphaExpr e3
  e1 <- withEnvPreserving $ alphaExpr e1
  modify $ \state -> state {usedVars = preservedVars}
  return $ Comprehension e1 (Just name') e2 e3

alphaExpr :: Expr' -> Alpha Expr'
alphaExpr e = withPos e <$> case value e of
  Lit lit -> return $ Lit lit
  Var name -> Var <$> useExistingVarName name
  Sub e1 e2 -> Sub <$> alphaExpr e1 <*> alphaExpr e2
  ListExt es -> ListExt <$> mapM alphaExpr es
  ListComp comp -> ListComp <$> alphaComprehension comp
  IterComp comp -> IterComp <$> alphaComprehension comp
  Call name es -> Call <$> useExistingFunName name <*> mapM alphaExpr es
  Cond e1 e2 e3 -> Cond <$> alphaExpr e1 <*> alphaExpr e2 <*> alphaExpr e3

alphaListShape :: ListShape -> Alpha ListShape
alphaListShape shape = case shape of
  NoneShape -> return NoneShape
  ListShape t n -> ListShape <$> alphaListShape t <*> alphaExpr n

alphaSentence :: Sentence' -> Alpha Sentence'
alphaSentence sentence = withPos sentence <$> case value sentence of
  If e body1 body2 -> do
    e <- alphaExpr e
    body1 <- withBreakingEnv $ mapM alphaSentence body1
    body2 <- withBreakingEnv $ mapM alphaSentence body2
    return $ If e body1 body2
  For name e body -> withBreakingEnv $ do
    name <- defineNewVarName name
    e <- alphaExpr e
    body <- mapM alphaSentence body
    return $ For name e body
  Define name t e -> do
    t <- alphaMaybeType t
    e <- alphaExpr e
    name <- defineNewVarName name
    return $ Define name t e
  Declare name t shape -> do
    t <- alphaMaybeType t
    shape <- alphaListShape shape
    name <- defineNewVarName name
    return $ Declare name t shape
  Assign name es e -> do
    name <- useExistingVarName name
    es <- mapM alphaExpr es
    e <- alphaExpr e
    return $ Assign name es e
  Assert e -> Assert <$> alphaExpr e
  Return e -> Return <$> alphaExpr e

alphaToplevelDecl :: ToplevelDecl' -> Alpha ToplevelDecl'
alphaToplevelDecl decl = withPos decl <$> case value decl of
  ConstDef name t e -> do
    name <- defineNewVarName name
    t <- withEnvPreserving $ alphaMaybeType t
    e <- withEnvPreserving $ alphaExpr e
    return $ ConstDef name t e
  FunDef name args ret body -> do
    name <- defineNewFunName name
    withNewEnv $ do
      args <- mapM (\(x, t) -> (,) <$> defineNewVarName x <*> alphaMaybeType t) args
      ret <- withEnvPreserving $ alphaMaybeType ret
      body <- mapM alphaSentence body
      return $ FunDef name args ret body
  FromImport path -> return $ FromImport path

run :: Program -> Either String Program
run prog = runAlpha $ do
  decls <- mapM alphaToplevelDecl (decls prog)
  return $ Program decls
