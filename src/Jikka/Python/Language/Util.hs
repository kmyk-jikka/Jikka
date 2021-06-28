module Jikka.Python.Language.Util where

import Control.Monad.Identity
import Jikka.Common.Location
import Jikka.Python.Language.Expr

constIntExp :: Integer -> Expr
constIntExp = Constant . ConstInt

constBoolExp :: Bool -> Expr
constBoolExp = Constant . ConstBool

mapExprArgumentsM :: Monad m => (Expr' -> m Expr') -> Arguments -> m Arguments
mapExprArgumentsM f args = do
  kwDefaults <- mapM (mapExprM f) (argsKwDefaults args)
  defaults <- mapM (mapExprM f) (argsDefaults args)
  return $
    args
      { argsKwDefaults = kwDefaults,
        argsDefaults = defaults
      }

mapExprComprehensionM :: Monad m => (Expr' -> m Expr') -> Comprehension -> m Comprehension
mapExprComprehensionM f comp = do
  iter <- mapExprM f (compIter comp)
  target <- mapExprM f (compIter comp)
  ifs <- mapM (mapExprM f) (compIfs comp)
  return $ Comprehension {compTarget = target, compIter = iter, compIfs = ifs}

mapExprComprehensionsM :: Monad m => (Expr' -> m Expr') -> [Comprehension] -> m [Comprehension]
mapExprComprehensionsM f = mapM (mapExprComprehensionM f)

mapExprKeywordsM :: Monad m => (Expr' -> m Expr') -> [Keyword'] -> m [Keyword']
mapExprKeywordsM f kwargs = mapM (\(WithLoc loc (k, v)) -> WithLoc loc . (,) k <$> mapExprM f v) kwargs

mapExprM :: Monad m => (Expr' -> m Expr') -> Expr' -> m Expr'
mapExprM f = go
  where
    go e0 =
      f . WithLoc (loc e0) =<< case value e0 of
        BoolOp e1 op e2 -> BoolOp <$> go e1 <*> pure op <*> go e2
        NamedExpr x e -> NamedExpr <$> go x <*> go e
        BinOp e1 op e2 -> BinOp <$> go e1 <*> pure op <*> go e2
        UnaryOp op e -> UnaryOp op <$> go e
        Lambda args body -> Lambda <$> mapExprArgumentsM f args <*> mapExprM f body
        IfExp e1 e2 e3 -> IfExp <$> go e1 <*> go e2 <*> go e3
        Dict es -> Dict <$> mapM (\(k, v) -> (,) <$> traverse go k <*> go v) es
        Set es -> Set <$> mapM go es
        ListComp e comps -> ListComp <$> mapExprM f e <*> mapExprComprehensionsM f comps
        SetComp e comps -> SetComp <$> mapExprM f e <*> mapExprComprehensionsM f comps
        DictComp k v comps -> DictComp <$> mapExprM f k <*> mapExprM f v <*> mapExprComprehensionsM f comps
        GeneratorExp e comps -> GeneratorExp <$> mapExprM f e <*> mapExprComprehensionsM f comps
        Await e -> Await <$> go e
        Yield e -> Yield <$> traverse go e
        YieldFrom e -> YieldFrom <$> go e
        Compare e es -> Compare <$> go e <*> mapM (\(op, e) -> (,) op <$> go e) es
        Call g args kwargs -> Call <$> go g <*> mapM go args <*> mapExprKeywordsM f kwargs
        FormattedValue e1 c e2 -> FormattedValue <$> go e1 <*> pure c <*> traverse go e2
        JoinedStr es -> JoinedStr <$> mapM go es
        Constant constant -> return $ Constant constant
        Attribute e x -> Attribute <$> go e <*> pure x
        Subscript e1 e2 -> Subscript <$> go e1 <*> go e2
        Starred e -> Starred <$> go e
        Name x -> return $ Name x
        List es -> List <$> mapM go es
        Tuple es -> Tuple <$> mapM go es
        Slice e1 e2 e3 -> Slice <$> traverse go e1 <*> traverse go e2 <*> traverse go e3

mapExprExceptHanderM :: Monad m => (Expr' -> m Expr') -> ExceptHandler' -> m ExceptHandler'
mapExprExceptHanderM f (WithLoc loc handler) = do
  body <- mapExprStatementsM f (exchBody handler)
  return $ WithLoc loc (handler {exchBody = body})

mapExprStatementM :: Monad m => (Expr' -> m Expr') -> Statement' -> m Statement'
mapExprStatementM f stmt =
  WithLoc (loc stmt) <$> case value stmt of
    FunctionDef g args body decorators ret -> FunctionDef g args <$> mapExprStatementsM f body <*> mapM (mapExprM f) decorators <*> pure ret
    AsyncFunctionDef g args body decorators ret -> AsyncFunctionDef g args <$> mapExprStatementsM f body <*> mapM (mapExprM f) decorators <*> pure ret
    ClassDef name bases keywords body decorators -> ClassDef name <$> mapM (mapExprM f) bases <*> mapExprKeywordsM f keywords <*> mapExprStatementsM f body <*> mapM (mapExprM f) decorators
    Return e -> Return <$> traverse (mapExprM f) e
    Delete xs -> Delete <$> mapM (mapExprM f) xs
    Assign xs e -> Assign <$> mapM (mapExprM f) xs <*> mapExprM f e
    AugAssign x op e -> AugAssign <$> mapExprM f x <*> pure op <*> mapExprM f e
    AnnAssign x t e -> AnnAssign <$> mapExprM f x <*> mapExprM f t <*> traverse (mapExprM f) e
    For x iter body orelse -> For <$> mapExprM f x <*> mapExprM f iter <*> mapExprStatementsM f body <*> mapExprStatementsM f orelse
    AsyncFor x iter body orelse -> AsyncFor <$> mapExprM f x <*> mapExprM f iter <*> mapExprStatementsM f body <*> mapExprStatementsM f orelse
    While e body orelse -> While <$> mapExprM f e <*> mapExprStatementsM f body <*> mapExprStatementsM f orelse
    If e body orelse -> If <$> mapExprM f e <*> mapExprStatementsM f body <*> mapExprStatementsM f orelse
    With withitems body -> With <$> mapM (\(e1, e2) -> (,) <$> mapExprM f e1 <*> traverse (mapExprM f) e2) withitems <*> mapExprStatementsM f body
    AsyncWith withitems body -> AsyncWith <$> mapM (\(e1, e2) -> (,) <$> mapExprM f e1 <*> traverse (mapExprM f) e2) withitems <*> mapExprStatementsM f body
    Raise e1 e2 -> Raise <$> traverse (mapExprM f) e1 <*> traverse (mapExprM f) e2
    Try body handlers orelse finalbody -> Try <$> mapExprStatementsM f body <*> mapM (mapExprExceptHanderM f) handlers <*> mapExprStatementsM f orelse <*> mapExprStatementsM f finalbody
    Assert e1 e2 -> Assert <$> mapExprM f e1 <*> traverse (mapExprM f) e2
    Import aliases -> return $ Import aliases
    ImportFrom xs aliases -> return $ ImportFrom xs aliases
    Global xs -> return $ Global xs
    Nonlocal xs -> return $ Nonlocal xs
    Expr' e -> Expr' <$> mapExprM f e
    Pass -> return Pass
    Break -> return Break
    Continue -> return Continue

mapExprStatementsM :: Monad m => (Expr' -> m Expr') -> [Statement'] -> m [Statement']
mapExprStatementsM f = mapM (mapExprStatementM f)

mapExprProgramM :: Monad m => (Expr' -> m Expr') -> Program -> m Program
mapExprProgramM = mapExprStatementsM

mapExprProgram :: (Expr' -> Expr') -> Program -> Program
mapExprProgram f = runIdentity . mapExprStatementsM (return . f)
