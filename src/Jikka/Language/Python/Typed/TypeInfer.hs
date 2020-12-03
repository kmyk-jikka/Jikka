module Jikka.Language.Python.Typed.TypeInfer (TypeEnv (..), emptyTypeEnv, infer', infer, check', check) where

import Jikka.Language.Common.Name
import Jikka.Language.Python.Typed.Expr
import Jikka.Language.Python.Typed.Stdlib

--------------------------------------------------------------------------------
-- inference

data TypeEnv
  = TypeEnv
      { funenv :: [(FunName, ([Type], Type))],
        varenv :: [(VarName, Type)]
      }
  deriving (Eq, Ord, Show, Read)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv [] []

infer' :: TypeEnv -> Expr -> Either String Type
infer' env e = case e of
  Var name -> case lookup name (varenv env) of
    Nothing -> Left $ "undefined variable: " ++ show (unVarName name)
    Just t -> Right t
  Lit lit -> Right $ literalType lit
  UnOp op _ -> let (_, ret) = unaryOpType op in Right ret
  BinOp op _ _ -> let (_, _, ret) = binaryOpType op in Right ret
  TerOp op _ _ _ -> let (_, _, _, ret) = ternaryOpType op in Right ret
  Sub t _ _ -> Right t
  ListExt t _ -> Right $ ATyList t
  ListComp (Comprehension t1 _ _ _ _ _) -> Right $ ATyList t1
  IterComp (Comprehension t1 _ _ _ _ _) -> Right $ ATyIterator t1
  Call name _ -> case lookup name (funenv env) of
    Nothing -> Left $ "undefined function: " ++ show (unFunName name)
    Just (_, ret) -> Right ret

-- | 'infer' extracts the type of the given expr, under the assumption that the expr is correctly typed.
infer :: TypeEnv -> Expr -> Type
infer env e = case infer' env e of
  Right t -> t
  Left msg -> error $ "failed to infer the type of: " ++ show e ++ ": " ++ msg

--------------------------------------------------------------------------------
-- checking

checkSubtype :: Type -> Type -> Either String ()
checkSubtype t1 t2 = case (t1, t2) of
  _ | toChurchType t1 == toChurchType t2 -> Right ()
  (ATyList t1, ATyIterator t2) -> checkSubtype t1 t2
  (ATyArray t1 _, ATyIterator t2) -> checkSubtype t1 t2
  _ -> Left $ "incompatible types: " ++ show t1 ++ " and " ++ show t2

checkSubtype' :: TypeEnv -> Expr -> Type -> Either String ()
checkSubtype' env e t = do
  t' <- check' env e
  checkSubtype t' t

checkCompresion :: TypeEnv -> Comprehension -> Either String Type
checkCompresion env (Comprehension t1 e1 name t e2 e3) = do
  checkSubtype' env e2 (ATyIterator t)
  let env' = env {varenv = (name, t) : varenv env}
  case e3 of
    Nothing -> return ()
    Just e3 -> checkSubtype' env' e3 ATyBool
  checkSubtype' env' e1 t1
  return t1

check' :: TypeEnv -> Expr -> Either String Type
check' env e = case e of
  Var name -> case lookup name (varenv env) of
    Nothing -> Left $ "undefined variable: " ++ show (unVarName name)
    Just t -> return t
  Lit lit -> return $ literalType lit
  UnOp op e1 -> do
    let (t1, ret) = unaryOpType op
    checkSubtype' env e1 t1
    return ret
  BinOp op e1 e2 -> do
    let (t1, t2, ret) = binaryOpType op
    checkSubtype' env e1 t1
    checkSubtype' env e2 t2
    return ret
  TerOp op e1 e2 e3 -> do
    let (t1, t2, t3, ret) = ternaryOpType op
    checkSubtype' env e1 t1
    checkSubtype' env e2 t2
    checkSubtype' env e3 t3
    return ret
  Sub t e1 e2 -> do
    checkSubtype' env e1 (ATyList t)
    checkSubtype' env e2 ATyInt
    return t
  ListExt t es -> do
    mapM_ (\e -> checkSubtype' env e t) es
    return $ ATyList t
  ListComp comp -> ATyList <$> checkCompresion env comp
  IterComp comp -> ATyIterator <$> checkCompresion env comp
  Call name es -> case lookup name (funenv env) of
    Nothing -> Left $ "undefined function: " ++ show (unFunName name)
    Just (ts, _) | length ts /= length es -> Left $ "the number of argument mismatch for function: " ++ show (unFunName name)
    Just (ts, ret) -> do
      mapM_ (\(e, t) -> checkSubtype' env e t) (zip es ts)
      return ret

-- | 'check' calculates the type of the given expr, with strict checking about their Church-style types. This function says nothing about Curry-style types.
check :: TypeEnv -> Expr -> Either String ChurchType
check env e = toChurchType <$> check' env e
