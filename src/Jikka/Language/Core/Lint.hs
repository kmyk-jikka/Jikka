module Jikka.Language.Core.Lint where

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import Jikka.Language.Core.Type

unitType :: Type
unitType = TyConApp UnitTyCon []

boolType :: Type
boolType = TyConApp BoolTyCon []

intType :: Type
intType = TyConApp IntTyCon []

listType :: Type -> Type
listType t = TyConApp ListTyCon [t]

litType :: Literal -> Type
litType lit = case lit of
  Unit -> unitType
  Bool _ -> boolType
  Int _ -> intType

returnTy :: Type -> Type -> Maybe Type
returnTy (FunTy t1 t2) t3 | t1 == t3 = Just t2
returnTy _ _ = Nothing

-- | 'exprType' assumes the input expr is well-typed
exprType :: Expr -> Type
exprType e = case e of
  Var x -> varType x
  Lit lit -> litType lit
  App e1 e2 -> do
    let t1 = exprType e1
    let t2 = exprType e2
    case returnTy t1 t2 of
      Just t -> t
      Nothing -> error ("type mismatch: cannot apply " ++ show t1 ++ " to " ++ show t2)
  Lam x e -> FunTy (varType x) (exprType e)
  Let _ e -> exprType e
  Case _ _ t _ -> t

type ErrorMessage = String

validateTrue :: Bool -> String -> Either String ()
validateTrue True _ = Right ()
validateTrue False err = Left err

validateEqualType :: Type -> Type -> Either String ()
validateEqualType t1 t2 = validateTrue (t1 == t2) ("type mismatch: " ++ show t1 ++ " and " ++ show t2)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing = Left a
maybeToEither _ (Just b) = Right b

validateExistingName :: M.Map Name Var -> Name -> Either String Type
validateExistingName env x' = do
  x <- maybeToEither ("symbol not found: " ++ show x') $ M.lookup x' env
  return $ varType x

validateExistingVar :: M.Map Name Var -> Var -> Either String Type
validateExistingVar env x = do
  t <- validateExistingName env (varName x)
  validateEqualType t (varType x)
  return t

validateNewVar :: M.Map Name Var -> Var -> Either String Type
validateNewVar env x = do
  validateTrue (varName x `M.notMember` env) ("symbol already defined: " ++ show (varName x))
  return $ varType x

validateAlt :: M.Map Name Var -> Type -> Alt -> Either String ()
validateAlt env t (altcon, vars, e) = case altcon of
  DataAlt datacon -> undefined -- TODO
  LitAlt lit -> do
    validateEqualType t (litType lit)
    _ <- validateExpr env e
    return ()
  DEFAULT -> do
    _ <- validateExpr env e
    return ()

validateExpr :: M.Map Name Var -> Expr -> Either String Type
validateExpr env e = case e of
  Var x ->
    validateExistingVar env x
  Lit lit ->
    return $ litType lit
  App e1 e2 -> do
    t1 <- validateExpr env e1
    t2 <- validateExpr env e2
    case returnTy t1 t2 of
      Nothing -> Left ("type mismatch: cannot apply " ++ show t1 ++ " to " ++ show t2)
      Just t -> return t
  Lam x e -> do
    t1 <- validateNewVar env x
    let env' = M.insert (varName x) x env
    t2 <- validateExpr env' e
    return $ FunTy t1 t2
  Let (NonRec x e1) e2 -> do
    t1 <- validateNewVar env x
    t1' <- validateExpr env e1
    validateEqualType t1 t1'
    validateExpr env e
  Let (Rec xes) e2 ->
    undefined -- TODO
  Case e1 x t alts -> do
    t1 <- validateExpr env e1
    t1' <- validateNewVar env x
    validateEqualType t1 t1'
    let env' = M.insert (varName x) x env
    forM_ alts (validateAlt env' t1)
    return t
