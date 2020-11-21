module Jikka.Language.Python.Typed.TypeInfer (TypeEnv (..), emptyTypeEnv, infer', infer, check', check) where

import Data.List (lookup)
import Jikka.Language.Common.Name
import Jikka.Language.Python.Typed.Stdlib
import Jikka.Language.Python.Typed.Type

--------------------------------------------------------------------------------
-- basics

intersectionType :: (Eq expr, Show expr) => CurryType expr -> CurryType expr -> Either String (CurryType expr)
intersectionType t1 t2 = case (t1, t2) of
  _ | t1 == t2 -> Right t1
  _ | toChurchType t1 == toChurchType t2 -> Right $ toCurryType (toChurchType t1)
  (ATyList t1, ATyIterator t2) -> ATyIterator <$> intersectionType t1 t2
  (ATyIterator t1, ATyList t2) -> ATyIterator <$> intersectionType t1 t2
  (ATyArray t1 _, ATyIterator t2) -> ATyIterator <$> intersectionType t1 t2
  (ATyIterator t1, ATyArray t2 _) -> ATyIterator <$> intersectionType t1 t2
  (ATyVar _, _) -> Left $ "we cannot make a intersection type for a type variable: " ++ show t1
  (_, ATyVar _) -> Left $ "we cannot make a intersection type for a type variable: " ++ show t2
  _ -> Left $ "there is no intersection type: " ++ show t1 ++ " and " ++ show t2

elementType :: Eq expr => CurryType expr -> Maybe (CurryType expr)
elementType (ATyList t) = Just t
elementType (ATyIterator t) = Just t
elementType (ATyArray t _) = Just t
elementType _ = Nothing

literalType :: Literal -> CurryType expr
literalType lit = case lit of
  LitInt n -> if n >= 0 then ATyNat else ATyInt
  LitBool p -> ATyBool

unaryOpType :: UnaryOp -> (CurryType expr, CurryType expr)
unaryOpType op =
  let ii = (ATyInt, ATyInt)
      nn = (ATyNat, ATyNat)
      bb = (ATyBool, ATyBool)
   in case op of
        -- arithmetical functions
        Negate -> ii
        Fact -> nn
        Abs -> (ATyInt, ATyNat)
        -- logical functions
        Not -> bb
        -- bitwise functions
        BitNot -> ii
        -- list functions
        Len t -> let t' = toCurryType t in (ATyList t', ATyNat)
        Sum -> (ATyIterator ATyInt, ATyInt)
        Product -> (ATyIterator ATyInt, ATyInt)
        Min1 -> (ATyIterator ATyInt, ATyInt)
        Max1 -> (ATyIterator ATyInt, ATyInt)
        ArgMin -> (ATyIterator ATyInt, ATyNat)
        ArgMax -> (ATyIterator ATyInt, ATyNat)
        All -> (ATyIterator ATyBool, ATyBool)
        Any -> (ATyIterator ATyBool, ATyBool)
        Sorted t -> let t' = toCurryType t in (ATyList t', ATyList t')
        List t -> let t' = toCurryType t in (ATyIterator t', ATyList t')
        Reversed t -> let t' = toCurryType t in (ATyIterator t', ATyList t')
        Range1 -> (ATyInt, ATyIterator ATyNat)

binaryOpType :: BinaryOp -> (CurryType expr, CurryType expr, CurryType expr)
binaryOpType op =
  let iii = (ATyInt, ATyInt, ATyInt)
      bbb = (ATyInt, ATyInt, ATyInt)
      inn = (ATyInt, ATyNat, ATyNat)
      iib = (ATyBool, ATyBool, ATyInt)
      nnn = (ATyNat, ATyNat, ATyNat)
   in case op of
        -- arithmetical functions
        Plus -> iii
        Minus -> iii
        Mult -> iii
        FloorDiv -> iii
        FloorMod -> iii
        CeilDiv -> iii
        CeilMod -> iii
        Pow -> iii
        Gcd -> iii
        Lcm -> iii
        Min -> iii
        Max -> iii
        -- modular functions
        Inv -> inn
        -- combinational functions
        Choose -> nnn
        Permute -> nnn
        MultiChoose -> nnn
        -- logical functions
        And -> bbb
        Or -> bbb
        Implies -> bbb
        -- bitwise functions
        BitAnd -> iii
        BitOr -> iii
        BitXor -> iii
        BitLeftShift -> iii
        BitRightShift -> iii
        -- list functions
        Range2 -> (ATyInt, ATyInt, ATyIterator ATyInt)
        -- arithmetical relations
        LessThan -> iib
        LessEqual -> iib
        GreaterThan -> iib
        GreaterEqual -> iib
        -- equality relations (polymorphic)
        Equal t -> let t' = toCurryType t in (t', t', ATyBool)
        NotEqual t -> let t' = toCurryType t in (t', t', ATyBool)

ternaryOpType :: TernaryOp -> (CurryType expr, CurryType expr, CurryType expr, CurryType expr)
ternaryOpType op = case op of
  Cond t -> let t' = toCurryType t in (ATyBool, t', t', t')
  PowMod -> (ATyInt, ATyInt, ATyNat, ATyNat)
  Range3 -> (ATyInt, ATyInt, ATyInt, ATyIterator ATyInt)

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
  ListComp t _ _ _ _ -> Right $ ATyList t
  ListExt t _ -> Right $ ATyList t
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
  ListComp t e1 name e2 e3 -> do
    checkSubtype' env e2 (ATyIterator t)
    let env' = env {varenv = (name, t) : varenv env}
    case e3 of
      Nothing -> return ()
      Just e3 -> checkSubtype' env' e3 ATyBool
    checkSubtype' env' e1 t
    return $ ATyList t
  ListExt t es -> do
    mapM_ (\e -> checkSubtype' env e t) es
    return $ ATyList t
  Call name es -> case lookup name (funenv env) of
    Nothing -> Left $ "undefined function: " ++ show (unFunName name)
    Just (ts, _) | length ts /= length es -> Left $ "the number of argument mismatch for function: " ++ show (unFunName name)
    Just (ts, ret) -> do
      mapM_ (\(e, t) -> checkSubtype' env e t) (zip es ts)
      return ret

-- | 'check' calculates the type of the given expr, with strict checking about their Church-style types. This function says nothing about Curry-style types.
check :: TypeEnv -> Expr -> Either String ChurchType
check env e = toChurchType <$> check' env e
