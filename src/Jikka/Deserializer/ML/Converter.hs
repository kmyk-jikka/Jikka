module Jikka.Deserializer.ML.Converter (run) where

import Control.Monad.Except (Except, runExcept, throwError)
import qualified Control.Monad.State.Strict as S
import Control.Monad.State.Strict (StateT, evalStateT)
import qualified Data.Map.Strict as M
import Jikka.Deserializer.ML.Pos
import Jikka.Deserializer.ML.Type
import qualified Jikka.Language.Parsed.Type as J

builtInMap :: M.Map Name J.BuiltIn
builtInMap =
  M.fromList
    [ ("not", J.Not),
      ("&&", J.And),
      ("||", J.Or),
      ("+", J.Add),
      ("-", J.Sub),
      ("*", J.Mul),
      ("/", J.Div),
      ("%", J.Mod),
      ("**", J.Pow),
      ("min", J.Min),
      ("max", J.Max),
      ("=", J.Equal),
      ("<>", J.NotEqual),
      ("<", J.LessThan),
      ("<=", J.LessThanEqual),
      (">", J.GreaterThan),
      (">=", J.GreaterThanEqual)
    ]

optName :: Maybe Name -> M Name
optName Nothing = newName
optName (Just x) = return x

var :: WithPos Name -> M J.Expr
var x = do
  (_, env) <- S.get
  case M.lookup (value x) env of
    Just _ -> return $ J.Var (value x)
    Nothing -> case M.lookup (value x) builtInMap of
      Just builtIn -> return $ J.BuiltIn builtIn
      Nothing -> throwError $ "Syntax error at " ++ prettyPos (pos x) ++ ": undefined identifier `" ++ value x ++ "'"

type' :: Type -> J.Type
type' t = case t of
  TyVar "unit" -> J.TyUnit
  TyVar "int" -> J.TyInt
  TyVar "bool" -> J.TyBool
  TyVar t -> J.TyVar t
  TyFun t1 t2 -> J.TyFun (type' t1) (type' t2)

optType :: Maybe (WithPos Type) -> M J.Type
optType Nothing = J.TyVar <$> newName
optType (Just WithPos {value = t}) = return $ type' t

optArgs :: [(Maybe Name, Maybe (WithPos Type))] -> M [(Name, J.Type)]
optArgs = mapM go
  where
    go (x, t) = (,) <$> optName x <*> optType t

funType :: [J.Type] -> Maybe (WithPos Type) -> M J.Type
funType ts t = do
  t <- optType t
  return $ foldr J.TyFun t ts

literal :: Literal -> J.Literal
literal lit = case lit of
  Unit -> J.Unit
  Bool p -> J.Bool p
  Int n -> J.Int n

singleBranch :: [(Name, J.Type)] -> WithPos Expr -> M ([J.Pattern], J.Expr)
singleBranch args e = do
  let pats = map (J.PatVar . fst) args
  e <- pushEnvs args (expr e)
  return (pats, e)

matchPattern :: MatchPattern -> M J.Pattern
matchPattern pat = case pat of
  PatVar x -> J.PatVar <$> optName x
  PatLit lit -> return $ J.PatLit (literal lit)
  PatPlusK x k -> J.PatPlusK <$> optName x <*> pure k

matchBranch :: ([MatchPattern], WithPos Expr) -> M ([J.Pattern], J.Expr)
matchBranch (pats, e) = do
  pats <- mapM matchPattern pats
  let go pat = case pat of
        J.PatVar x -> (\t -> [(x, J.TyVar t)]) <$> newName
        J.PatLit _ -> return []
        J.PatPlusK x _ -> (\t -> [(x, J.TyVar t)]) <$> newName
  xts <- concat <$> mapM go pats
  e <- pushEnvs xts $ expr e
  return (pats, e)

expr :: WithPos Expr -> M J.Expr
expr e = case value e of
  Lit lit -> return $ J.Lit (literal lit)
  Var x -> var (withPos e x)
  App e1 e2 -> J.App <$> expr e1 <*> expr e2
  Let ltype x args t e1 e2 -> do
    x <- optName x
    args <- optArgs args
    t <- funType (map snd args) t
    case (ltype, args) of
      (NoRec, []) -> do
        e1 <- expr e1
        e2 <- pushEnv x t (expr e2)
        return $ J.Let x t e1 e2
      (NoRec, _) -> do
        e1 <- J.Fun J.NoRec . (: []) <$> singleBranch args e1
        e2 <- pushEnv x t (expr e2)
        return $ J.Let x t e1 e2
      (Rec, []) -> pushEnv x t $ do
        e1 <- expr e1
        e2 <- expr e2
        let e1' = J.App (J.Fun (J.Rec x) [([J.PatLit J.Unit], e1)]) (J.Lit J.Unit)
        return $ J.Let x t e1' e2
      (Rec, _) -> pushEnv x t $ do
        e1 <- J.Fun (J.Rec x) . (: []) <$> singleBranch args e1
        e2 <- expr e2
        return $ J.Let x t e1 e2
  Fun args e -> do
    args <- optArgs args
    J.Fun J.NoRec . (: []) <$> singleBranch args e
  If e1 e2 e3 -> do
    e1 <- expr e1
    e2 <- expr e2
    e3 <- expr e3
    return $ J.App (J.App (J.App (J.BuiltIn J.If) e1) e2) e3
  Match e bs -> J.App <$> (J.Fun J.NoRec <$> mapM matchBranch bs) <*> expr e
  Function bs -> J.Fun J.NoRec <$> mapM matchBranch bs

type M = StateT (Int, M.Map Name J.Type) (Except String)

newName :: M Name
newName = S.state (\(n, env) -> ('_' : show (n + 1), (n + 1, env)))

pushEnv :: Name -> J.Type -> M a -> M a
pushEnv x t = pushEnvs [(x, t)]

pushEnvs :: [(Name, J.Type)] -> M a -> M a
pushEnvs xts f = do
  (_, env) <- S.get
  let env' = foldl (\env (x, t) -> M.insert x t env) env xts
  S.modify (\(n, _) -> (n, env'))
  value <- f
  S.modify (\(n, _) -> (n, env))
  return value

runM :: M a -> Either String a
runM f = runExcept $ evalStateT f (0, M.empty)

run :: Program -> Either String J.Program
run prog = runM $ do
  let f (x, t) = (x, type' $ value t)
  let given' = map f $ given prog
  pushEnvs given' $ do
    body <- expr $ body prog
    return
      J.Program
        { J.given = given',
          J.body = body
        }
