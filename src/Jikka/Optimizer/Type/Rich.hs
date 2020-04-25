module Jikka.Optimizer.Type.Rich where

import Control.Arrow ((&&&), (***))
import Control.Monad.ST
import qualified Data.Map.Strict as M
import Data.STRef
import qualified Data.Set as S
import Jikka.Optimizer.Type.Common (Name)
import qualified Jikka.Optimizer.Type.Simple as S
import Jikka.Optimizer.Type.Simple (FunType (..), Literal (..), Pattern (..))

data MonoType name
  = TyVar name
  | TyUnit
  | TyBool
  | TyInt
  | TyFun (MonoType name) (MonoType name)
  deriving (Eq, Ord, Read, Show)

data PolyType name
  = Mono (MonoType name)
  | Forall name (PolyType name)
  deriving (Eq, Ord, Read, Show)

data BuiltIn
  = Not
  | And
  | Or
  | If
  | Succ
  | Pred
  | Neg
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Min
  | Max
  | Sum
  | Product
  | Minimum
  | Maximum
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

freeVars :: Ord name => MonoType name -> S.Set name
freeVars = S.fromList . go []
  where
    go :: [name] -> MonoType name -> [name]
    go acc (TyVar a) = a : acc
    go acc (TyFun t t') = go (go acc t) t'
    go acc _ = acc

universalClosure :: Ord name => MonoType name -> PolyType name
universalClosure t = foldr Forall (Mono t) (freeVars t)

builtInInfo :: BuiltIn -> (Name, PolyType Name)
builtInInfo op =
  let fun1 = TyFun
      fun2 x y z = TyFun x (TyFun y z)
      fun3 w x y z = TyFun w (TyFun x (TyFun y z))
      bool = TyBool
      int = TyInt
      a = TyVar "a"
      cont (x, t) = (x, universalClosure t)
   in cont $ case op of
        Not -> ("not", fun1 bool bool)
        And -> ("and", fun2 bool bool bool)
        Or -> ("or", fun2 bool bool bool)
        If -> ("if", fun3 bool a a a)
        Succ -> ("succ", fun1 int int)
        Pred -> ("pred", fun1 int int)
        Neg -> ("neg", fun1 int int)
        Add -> ("+", fun2 int int int)
        Sub -> ("-", fun2 int int int)
        Mul -> ("*", fun2 int int int)
        Div -> ("/", fun2 int int int)
        Mod -> ("%", fun2 int int int)
        Min -> ("min", fun2 int int int)
        Max -> ("max", fun2 int int int)
        Sum -> ("sum", fun2 (fun1 int int) int int)
        Product -> ("product", fun2 (fun1 int int) int int)
        Minimum -> ("minimum", fun2 (fun1 int int) int int)
        Maximum -> ("maximum", fun2 (fun1 int int) int int)

builtInName :: BuiltIn -> Name
builtInName = fst . builtInInfo

builtInType :: BuiltIn -> PolyType Name
builtInType = snd . builtInInfo

builtInEnv :: M.Map Name BuiltIn
builtInEnv = M.fromList $ map (builtInName &&& id) [minBound .. maxBound]

builtInFromName :: Name -> Maybe BuiltIn
builtInFromName name = M.lookup name builtInEnv

literalType :: Literal -> MonoType name
literalType l = case l of
  Unit -> TyUnit
  Bool _ -> TyBool
  Int _ -> TyInt

newtype MonoTypeRef s = MonoTypeRef {unMonoTypeRef :: MonoType (STRef s (Maybe (MonoTypeRef s)))}
  deriving (Eq)

replaceTypeVarsWithRefs :: Ord name => STRef s (M.Map name (STRef s (Maybe (MonoTypeRef s)))) -> MonoType name -> ST s (MonoTypeRef s)
replaceTypeVarsWithRefs env t = MonoTypeRef <$> case t of
  TyVar name -> do
    env' <- readSTRef env
    r <- case M.lookup name env' of
      Just r -> return r
      Nothing -> newSTRef Nothing
    return $ TyVar r
  TyUnit -> return TyUnit
  TyBool -> return TyBool
  TyInt -> return TyInt
  TyFun t1 t2 -> do
    t1' <- unMonoTypeRef <$> replaceTypeVarsWithRefs env t1
    t2' <- unMonoTypeRef <$> replaceTypeVarsWithRefs env t2
    return $ TyFun t1' t2'

data Expr t
  = Lit Literal
  | Var Name
  | BuiltIn BuiltIn
  | Let Name t (Expr t) (Expr t)
  | Fun FunType [([Pattern], Expr t)]
  | App (Expr t) (Expr t)
  deriving (Eq, Ord, Read, Show)

replaceTypeVarsWithRefsInExpr :: Ord name => STRef s (M.Map name (STRef s (Maybe (MonoTypeRef s)))) -> Expr (MonoType name) -> ST s (Expr (MonoTypeRef s))
replaceTypeVarsWithRefsInExpr env e = case e of
  Lit l -> return $ Lit l
  Var x -> return $ Var x
  BuiltIn op -> return $ BuiltIn op
  Let x t e1 e2 -> do
    t' <- replaceTypeVarsWithRefs env t
    e1' <- replaceTypeVarsWithRefsInExpr env e1
    e2' <- replaceTypeVarsWithRefsInExpr env e2
    return $ Let x t' e1' e2'
  Fun ftype patterns -> do
    let go (args, body) = do
          body' <- replaceTypeVarsWithRefsInExpr env body
          return (args, body')
    patterns' <- mapM go patterns
    return $ Fun ftype patterns'
  App e1 e2 -> do
    e1' <- replaceTypeVarsWithRefsInExpr env e1
    e2' <- replaceTypeVarsWithRefsInExpr env e2
    return $ App e1' e2'
