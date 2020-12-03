module Jikka.Converter.Python.TypeInfer (run) where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Jikka.Language.Common.Name
import Jikka.Language.Python.Typed.Expr
import Jikka.Language.Python.Typed.Stdlib

--------------------------------------------------------------------------------
-- prepare a monad

data TypingState
  = TypingState
      { varenv :: [(VarName, Type)],
        funenv :: [(FunName, ([Type], Type))],
        subst :: M.Map TypeName Type
      }

type Typing a = ExceptT String (State TypingState) a

initialState :: TypingState
initialState =
  TypingState
    { varenv = [],
      funenv = [],
      subst = M.empty
    }

runTyping :: Typing a -> Either String a
runTyping a = evalState (runExceptT a) initialState

--------------------------------------------------------------------------------
-- prepare utilities

lookupVar :: VarName -> Typing Type
lookupVar name = do
  t <- lookup name <$> gets varenv
  case t of
    Nothing -> throwError $ "Internal Error: undefined variable: " ++ show (unVarName name)
    Just t -> return t

insertVar :: VarName -> Type -> Typing ()
insertVar name t =
  modify $ \state -> state {varenv = (name, t) : varenv state}

withNewVarEnv :: Typing a -> Typing a
withNewVarEnv cont = do
  varenv' <- gets varenv
  value <- cont
  modify $ \state -> state {varenv = varenv'}
  return value

withVar :: VarName -> Type -> Typing a -> Typing a
withVar name t cont = do
  varenv' <- gets varenv
  insertVar name t
  value <- cont
  varenv'' <- gets varenv
  if varenv'' == (name, t) : varenv'
    then modify $ \state -> state {varenv = varenv'}
    else throwError "Internal Error: varenv is broken"
  return value

lookupFun :: FunName -> Typing ([Type], Type)
lookupFun name = do
  t <- lookup name <$> gets funenv
  case t of
    Nothing -> throwError $ "Internal Error: undefined function: " ++ show (unFunName name)
    Just t -> return t

insertFun :: FunName -> [Type] -> Type -> Typing ()
insertFun name ts ret =
  modify $ \state -> state {funenv = (name, (ts, ret)) : funenv state}

lookupTyVar :: TypeName -> Typing (Maybe Type)
lookupTyVar name = do
  t <- M.lookup name <$> gets subst
  case t of
    Nothing -> return Nothing
    Just (ATyVar name') -> lookupTyVar name'
    Just t -> return $ Just t

insertTyVar' :: TypeName -> Type -> Typing ()
insertTyVar' name t =
  modify $ \state -> state {subst = M.insert name t (subst state)}

insertTyVar :: TypeName -> Type -> Typing ()
insertTyVar name t = do
  t' <- M.lookup name <$> gets subst
  case t' of
    Just (ATyVar name') -> insertTyVar name' t
    Just _ -> throwError $ "Internal Error: type variable already have a value: " ++ show (unTypeName name)
    Nothing -> insertTyVar' name t

--------------------------------------------------------------------------------
-- list type equations

assertSubtype :: Type -> Type -> Typing ()
assertSubtype t1 t2 = case (t1, t2) of
  _ | toChurchType t1 == toChurchType t2 -> return ()
  (_, ATyVar name) -> do
    t2' <- lookupTyVar name
    case t2' of
      Just t2' -> assertSubtype t1 t2'
      Nothing -> insertTyVar name t1 -- TODO: is this okay?
  (ATyVar name, _) -> do
    t1' <- lookupTyVar name
    case t1' of
      Just t1' -> assertSubtype t1' t2
      Nothing -> insertTyVar name t2 -- TODO: is this okay?
  _ | toChurchType t1 == toChurchType t2 -> return ()
  (ATyIterator t1', ATyIterator t2') -> assertSubtype t1' t2'
  (ATyList t1', ATyList t2') -> assertSubtype t1' t2'
  (ATyList t1', ATyArray t2' _) -> assertSubtype t1' t2'
  (ATyArray t1' _, ATyList t2') -> assertSubtype t1' t2'
  (ATyArray t1' _, ATyArray t2' _) -> assertSubtype t1' t2'
  (ATyList t1', ATyIterator t2') -> assertSubtype t1' t2'
  (ATyArray t1' _, ATyIterator t2') -> assertSubtype t1' t2'
  _ -> throwError $ "Internal Error: they are not in subtype relation: " ++ show (t1, t2)

unifyType :: Type -> Typing Type
unifyType t = case t of
  ATyInt -> return ATyInt
  ATyBool -> return ATyBool
  ATyList t -> ATyList <$> unifyType t
  ATyNat -> return ATyNat
  ATyInterval l r -> do
    (l, tl) <- unifyExpr l
    (r, tr) <- unifyExpr r
    assertSubtype tl ATyInt
    assertSubtype tr ATyInt
    return $ ATyInterval l r
  ATyIterator t -> ATyIterator <$> unifyType t
  ATyArray t n -> do
    (n, tn) <- unifyExpr n
    assertSubtype tn ATyNat
    ATyArray <$> unifyType t <*> return n
  ATyVar name -> return $ ATyVar name

unifyComprehension :: Comprehension -> Typing (Comprehension, Type)
unifyComprehension (Comprehension t1 e1 name t e2 e3) = do
  t1 <- unifyType t1
  t <- unifyType t
  (e2, t2) <- unifyExpr e2
  assertSubtype t2 (ATyIterator t)
  withVar name t $ do
    (e1, t1') <- unifyExpr e1
    assertSubtype t1' t1
    e3 <- case e3 of
      Nothing -> return Nothing
      Just e3 -> do
        (e3, t3) <- unifyExpr e3
        assertSubtype t3 ATyBool
        return $ Just e3
    return (Comprehension t1 e1 name t e2 e3, ATyList t1)

unifyExpr :: Expr -> Typing (Expr, Type)
unifyExpr e = case e of
  Var name -> do
    t <- lookupVar name
    return (Var name, t)
  Lit lit -> return (Lit lit, literalType lit)
  UnOp op e1 -> do
    let (t1', ret) = unaryOpType op
    (e1, t1) <- unifyExpr e1
    assertSubtype t1 t1'
    return (UnOp op e1, ret)
  BinOp op e1 e2 -> do
    let (t1', t2', ret) = binaryOpType op
    (e1, t1) <- unifyExpr e1
    (e2, t2) <- unifyExpr e2
    assertSubtype t1 t1'
    assertSubtype t2 t2'
    return (BinOp op e1 e2, ret)
  TerOp op e1 e2 e3 -> do
    let (t1', t2', t3', ret) = ternaryOpType op
    (e1, t1) <- unifyExpr e1
    (e2, t2) <- unifyExpr e2
    (e3, t3) <- unifyExpr e3
    assertSubtype t1 t1'
    assertSubtype t2 t2'
    assertSubtype t3 t3'
    return (TerOp op e1 e2 e3, ret)
  Sub t e1 e2 -> do
    (e1, t1) <- unifyExpr e1
    (e2, t2) <- unifyExpr e2
    assertSubtype t1 (ATyList t)
    assertSubtype t2 ATyNat
    return (Sub t e1 e2, t)
  ListExt t es -> do
    t <- unifyType t
    es <- forM es $ \e -> do
      (e, t') <- unifyExpr e
      assertSubtype t' t
      return e
    return (ListExt t es, ATyList t)
  ListComp comp -> do
    (comp, t) <- unifyComprehension comp
    return (ListComp comp, t)
  IterComp comp -> do
    (comp, t) <- unifyComprehension comp
    return (IterComp comp, t)
  Call name args -> do
    (ts, ret) <- lookupFun name
    if length ts == length args then return () else throwError "Internal Error: invalid funcall"
    args <- forM (zip args ts) $ \(e, t) -> do
      (e, t') <- unifyExpr e
      assertSubtype t' t
      return e
    return (Call name args, ret)

unifySentence :: Type -> Sentence -> Typing Sentence
unifySentence ret sentence = case sentence of
  If e body1 body2 -> do
    (e, t) <- unifyExpr e
    assertSubtype t ATyBool
    body1 <- withNewVarEnv $ do
      mapM (unifySentence ret) body1
    body2 <- withNewVarEnv $ do
      mapM (unifySentence ret) body2
    return $ If e body1 body2
  For name t e body -> do
    t <- unifyType t
    (e, t') <- unifyExpr e
    assertSubtype t' (ATyIterator t)
    body <- withNewVarEnv $ do
      insertVar name t
      mapM (unifySentence ret) body
    return $ For name t e body
  Declare name t shape -> do
    t <- unifyType t
    insertVar name t
    shape <- forM shape $ \e -> do
      (e, t') <- unifyExpr e
      assertSubtype t' ATyNat
      return e
    return $ Declare name t shape
  Assign name indices e -> do
    t <- lookupVar name
    indices <- forM indices $ \e' -> do
      (e', t') <- unifyExpr e'
      assertSubtype t' ATyNat
      return e'
    (e, t') <- unifyExpr e
    assertSubtype (foldr (\_ t' -> ATyList t') t' indices) t
    return $ Assign name indices e
  Define name t e -> do
    t <- unifyType t
    insertVar name t
    (e, t') <- unifyExpr e
    assertSubtype t' t
    return $ Define name t e
  Assert e -> do
    (e, t) <- unifyExpr e
    assertSubtype t ATyBool
    return $ Assert e
  Return e -> do
    (e, t) <- unifyExpr e
    assertSubtype t ret
    return $ Return e

unifyToplevelDecl :: ToplevelDecl -> Typing ToplevelDecl
unifyToplevelDecl decl = case decl of
  ConstDef name t e -> do
    t <- unifyType t
    (e, t') <- unifyExpr e
    assertSubtype t' t
    insertVar name t
    return $ ConstDef name t e
  FunDef name args ret body ->
    withNewVarEnv $ do
      args <- forM args $ \(name, t) -> do
        t <- unifyType t
        insertVar name t
        return (name, t)
      ret <- unifyType ret
      insertFun name (map snd args) ret
      body <- mapM (unifySentence ret) body
      return $ FunDef name args ret body

--------------------------------------------------------------------------------
-- solve type equations

substType' :: Type -> Typing Type
substType' t = case t of
  ATyInt -> return ATyInt
  ATyBool -> return ATyBool
  ATyList t -> ATyList <$> substType' t
  ATyNat -> return ATyNat
  ATyInterval l r -> ATyInterval <$> substExpr l <*> substExpr r
  ATyIterator t -> ATyIterator <$> substType' t
  ATyArray t n -> ATyArray <$> substType' t <*> substExpr n
  ATyVar name -> do
    t <- lookupTyVar name
    case t of
      Nothing -> throwError $ "Type Error: failed to guess type: " ++ show (unTypeName name)
      Just (ATyVar _) -> throwError $ "Type Error: failed to guess type: " ++ show (unTypeName name)
      Just t -> substType' t

substType :: Type -> Typing Type
substType t = stripLeakedVars =<< substType' t

substChurchType :: ChurchType -> Typing ChurchType
substChurchType t = toChurchType <$> substType (toCurryType t)

substUnaryOp :: UnaryOp -> Typing UnaryOp
substUnaryOp op = case op of
  Len t -> Len <$> substChurchType t
  Sorted t -> Sorted <$> substChurchType t
  List t -> List <$> substChurchType t
  Reversed t -> Reversed <$> substChurchType t
  _ -> return op

substBinaryOp :: BinaryOp -> Typing BinaryOp
substBinaryOp op = case op of
  Equal t -> Equal <$> substChurchType t
  NotEqual t -> NotEqual <$> substChurchType t
  _ -> return op

substTernaryOp :: TernaryOp -> Typing TernaryOp
substTernaryOp op = case op of
  Cond t -> Cond <$> substChurchType t
  _ -> return op

substComprehension :: Comprehension -> Typing Comprehension
substComprehension (Comprehension t1 e1 name t e2 e3) = do
  t1 <- substType t1
  t <- substType t
  e2 <- substExpr e2
  withVar name t $ do
    e1 <- substExpr e1
    e3 <- case e3 of
      Nothing -> return Nothing
      Just e3 -> Just <$> substExpr e3
    return $ Comprehension t1 e1 name t e2 e3

substExpr :: Expr -> Typing Expr
substExpr e = case e of
  Var name -> return $ Var name
  Lit lit -> return $ Lit lit
  UnOp op e1 -> UnOp <$> substUnaryOp op <*> substExpr e1
  BinOp op e1 e2 -> BinOp <$> substBinaryOp op <*> substExpr e1 <*> substExpr e2
  TerOp op e1 e2 e3 -> TerOp <$> substTernaryOp op <*> substExpr e1 <*> substExpr e2 <*> substExpr e3
  Sub t e1 e2 -> Sub <$> substType t <*> substExpr e1 <*> substExpr e2
  ListExt t es -> ListExt <$> substType t <*> mapM substExpr es
  ListComp comp -> ListComp <$> substComprehension comp
  IterComp comp -> IterComp <$> substComprehension comp
  Call name args -> Call name <$> mapM substExpr args

substSentence :: Sentence -> Typing Sentence
substSentence sentence = case sentence of
  If e body1 body2 -> do
    e <- substExpr e
    body1 <- withNewVarEnv $ do
      mapM substSentence body1
    body2 <- withNewVarEnv $ do
      mapM substSentence body2
    return $ If e body1 body2
  For name t e body -> do
    t <- substType t
    e <- substExpr e
    body <- withNewVarEnv $ do
      insertVar name t
      mapM substSentence body
    return $ For name t e body
  Declare name t shape -> do
    t <- substType t
    shape <- mapM substExpr shape
    insertVar name t
    return $ Declare name t shape
  Assign name indices e -> Assign name <$> mapM substExpr indices <*> substExpr e
  Define name t e -> do
    t <- substType t
    e <- substExpr e
    insertVar name t
    return $ Define name t e
  Assert e -> Assert <$> substExpr e
  Return e -> Return <$> substExpr e

substToplevelDecl :: ToplevelDecl -> Typing ToplevelDecl
substToplevelDecl decl = case decl of
  ConstDef name t e -> do
    t <- substType t
    e <- substExpr e
    return $ ConstDef name t e
  FunDef name args ret body -> do
    withNewVarEnv $ do
      args <- forM args $ \(name, t) -> do
        t <- substType t
        insertVar name t
        return (name, t)
      ret <- substType ret
      body <- mapM substSentence body
      return $ FunDef name args ret body

--------------------------------------------------------------------------------
-- stripping leaked variables

stripLeakedVars :: Type -> Typing Type
stripLeakedVars t = case t of
  ATyInt -> return ATyInt
  ATyBool -> return ATyBool
  ATyList t -> ATyList <$> stripLeakedVars t
  ATyNat -> return ATyNat
  ATyInterval l r -> do
    p <- hasLeakedVars l
    q <- hasLeakedVars r
    return $
      if p || q
        then ATyNat
        else ATyInterval l r
  ATyIterator t -> ATyIterator <$> stripLeakedVars t
  ATyArray t n -> do
    p <- hasLeakedVars n
    t <- stripLeakedVars t
    return $ if p then ATyList t else ATyArray t n
  ATyVar _ -> throwError "Internal Error: it's already type error"

hasLeakedVars :: Expr -> Typing Bool
hasLeakedVars e = case e of
  Var name -> do
    t <- lookup name <$> gets varenv
    return $ isNothing t
  Lit _ -> return False
  UnOp _ e1 -> hasLeakedVars e1
  BinOp _ e1 e2 -> do
    p1 <- hasLeakedVars e1
    p2 <- hasLeakedVars e2
    return $ p1 && p2
  TerOp _ e1 e2 e3 -> do
    p1 <- hasLeakedVars e1
    p2 <- hasLeakedVars e2
    p3 <- hasLeakedVars e3
    return $ p1 && p2 && p3
  Sub {} -> return False -- ignore complicated annotations
  ListExt _ _ -> return False -- ignore complicated annotations
  ListComp _ -> return False -- ignore complicated annotations
  IterComp _ -> return False -- ignore complicated annotations
  Call _ _ -> return False -- ignore complicated annotations

--------------------------------------------------------------------------------
-- typing AST

run :: Program -> Either String Program
run prog = runTyping $ do
  decls <- mapM unifyToplevelDecl (decls prog)
  decls <- mapM substToplevelDecl decls
  return $ Program decls
