{-# LANGUAGE LambdaCase #-}

module Jikka.Converter.Python.ToCore
  ( run,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable (foldlM)
import Data.List (lookup, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Traversable (forM)
import Jikka.Language.Common.Name
import qualified Jikka.Language.Core.Expr as Y
import qualified Jikka.Language.Core.Lint as Y (typecheckProgram')
import qualified Jikka.Language.Python.Typed.Stdlib as X
import qualified Jikka.Language.Python.Typed.Type as X

convertChurchType :: X.ChurchType -> Either String Y.Type
convertChurchType = \case
  X.TyInt -> return Y.IntTy
  X.TyBool -> return Y.BoolTy
  X.TyList t -> Y.ListTy <$> convertChurchType t
  X.TyIterator t -> Y.ListTy <$> convertChurchType t
  X.TyVar a -> Left $ "Internal Error: type variable found: " ++ show (unTypeName a)

convertType :: X.Type -> Either String Y.Type
convertType = convertChurchType . X.toChurchType

convertLiteral :: X.Literal -> Y.Literal
convertLiteral = \case
  X.LitInt n -> Y.LitInt n
  X.LitBool p -> Y.LitBool p

convertUnaryOp :: X.UnaryOp -> Either String Y.Expr
convertUnaryOp op = Y.Lit . Y.LitBuiltin <$> convertUnaryOp' op

convertUnaryOp' :: X.UnaryOp -> Either String Y.Builtin
convertUnaryOp' = \case
  X.Negate -> return Y.Negate
  X.Abs -> return Y.Abs
  X.Fact -> return Y.Fact
  X.Not -> return Y.Not
  X.BitNot -> return Y.BitNot
  X.Len t -> Y.Len <$> convertChurchType t
  X.Sum -> return Y.Sum
  X.Product -> return Y.Product
  X.Min1 -> return Y.Min1
  X.Max1 -> return Y.Max1
  X.ArgMin -> return Y.ArgMin
  X.ArgMax -> return Y.ArgMax
  X.All -> return Y.All
  X.Any -> return Y.Any
  X.Sorted t -> Y.Sorted <$> convertChurchType t
  X.List t -> Y.List <$> convertChurchType t
  X.Reversed t -> Y.Reversed <$> convertChurchType t
  X.Range1 -> return Y.Range1

convertBinaryOp :: X.BinaryOp -> Either String Y.Expr
convertBinaryOp op = Y.Lit . Y.LitBuiltin <$> convertBinaryOp' op

convertBinaryOp' :: X.BinaryOp -> Either String Y.Builtin
convertBinaryOp' = \case
  X.Plus -> return Y.Plus
  X.Minus -> return Y.Minus
  X.Mult -> return Y.Mult
  X.FloorDiv -> return Y.FloorDiv
  X.FloorMod -> return Y.FloorMod
  X.CeilDiv -> return Y.CeilDiv
  X.CeilMod -> return Y.CeilMod
  X.Pow -> return Y.Pow
  X.Gcd -> return Y.Gcd
  X.Lcm -> return Y.Lcm
  X.Min -> return Y.Min
  X.Max -> return Y.Max
  X.Inv -> return Y.Inv
  X.Choose -> return Y.Choose
  X.Permute -> return Y.Permute
  X.MultiChoose -> return Y.MultiChoose
  X.And -> return Y.And
  X.Or -> return Y.Or
  X.Implies -> return Y.Implies
  X.BitAnd -> return Y.BitAnd
  X.BitOr -> return Y.BitOr
  X.BitXor -> return Y.BitXor
  X.BitLeftShift -> return Y.BitLeftShift
  X.BitRightShift -> return Y.BitRightShift
  X.Range2 -> return Y.Range2
  X.LessThan -> return Y.LessThan
  X.LessEqual -> return Y.LessEqual
  X.GreaterThan -> return Y.GreaterThan
  X.GreaterEqual -> return Y.GreaterEqual
  X.Equal t -> Y.Equal <$> convertChurchType t
  X.NotEqual t -> Y.NotEqual <$> convertChurchType t

convertTernaryOp :: X.TernaryOp -> Either String Y.Expr
convertTernaryOp op = Y.Lit . Y.LitBuiltin <$> convertTernaryOp' op

convertTernaryOp' :: X.TernaryOp -> Either String Y.Builtin
convertTernaryOp' = \case
  X.Cond t -> Y.If <$> convertChurchType t
  X.PowMod -> return Y.PowMod
  X.Range3 -> return Y.Range3

convertComprehension :: X.Comprehension -> Either String Y.Expr
convertComprehension (X.Comprehension _ _ _ _ _ (Just _)) = Left "Internal Error: unsupproted form of comprehension"
convertComprehension (X.Comprehension t1 e1 x t e2 Nothing) = do
  t1 <- convertType t1
  e1 <- convertExpr e1
  t <- convertType t
  e2 <- convertExpr e2
  return $ Y.AppBuiltin (Y.Map t t1) [Y.Lam1 x t e1, e2]

convertExpr :: X.Expr -> Either String Y.Expr
convertExpr = \case
  X.Var x -> return $ Y.Var x
  X.Lit lit -> return $ Y.Lit (convertLiteral lit)
  X.UnOp op e1 -> Y.App <$> convertUnaryOp op <*> mapM convertExpr [e1]
  X.BinOp op e1 e2 -> Y.App <$> convertBinaryOp op <*> mapM convertExpr [e1, e2]
  X.TerOp op e1 e2 e3 -> Y.App <$> convertTernaryOp op <*> mapM convertExpr [e1, e2, e3]
  X.Sub t e1 e2 -> do
    t <- convertType t
    e1 <- convertExpr e1
    e2 <- convertExpr e2
    return $ Y.AppBuiltin (Y.At t) [e1, e2]
  X.ListExt _ _ -> Left "Internal Error: list literals are not supported"
  X.ListComp comp -> convertComprehension comp
  X.IterComp comp -> convertComprehension comp
  X.Call f args -> Y.App (Y.Var (VarName (unFunName f))) <$> mapM convertExpr args

convertDeclareFor :: VarName -> Y.Type -> [Y.Expr] -> [(VarName, Y.Expr)] -> X.Sentence -> Either String Y.Expr
convertDeclareFor x t shape cnts body = case (t, shape, body) of
  (t, [], X.Assign x' indices e) ->
    if x == x' && sort (map (X.Var . fst) cnts) == sort indices
      then do
        e <- convertExpr e
        indices <- forM indices $ \case
          X.Var x -> return x
          _ -> Left "Internal Error: index is not variable"
        let go (e, t) i = case lookup i cnts of
              Nothing -> Left "Internal Error: variable not found"
              Just size -> return (Y.AppBuiltin (Y.Tabulate t) [size, Y.Lam1 i Y.IntTy e], Y.ListTy t)
        (e, _) <- foldlM go (e, t) indices
        return e
      else Left "Internal Error: unsupported form of declare/for-loop found"
  (Y.ListTy t, _ : shape, X.For cnt t' (X.UnOp X.Range1 e) [body])
    | X.toChurchType t' == X.TyInt ->
      if isNothing (lookup cnt cnts)
        then do
          e <- convertExpr e
          convertDeclareFor x t shape ((cnt, e) : cnts) body
        else Left "Internal Error: name conflict at declare/for-loop"
  _ -> Left $ "Internal Error: invalid form of declare/for-loop found: " ++ show (t, shape, body)

convertSentences :: Y.Type -> [X.Sentence] -> Either String Y.Expr
convertSentences ret = \case
  [] -> Left "Internal Error: empty body"
  (X.If e body1 body2 : cont) -> do
    e <- convertExpr e
    body1 <- convertSentences ret (body1 ++ cont)
    body2 <- convertSentences ret (body2 ++ cont)
    return $ Y.AppBuiltin (Y.If ret) [e, body1, body2]
  (X.For {} : _) -> Left "Internal Error: unsupported form of for-loop found"
  (X.Declare x t shape : sentence : cont) -> do
    t <- convertType t
    shape <- mapM convertExpr shape
    e <- convertDeclareFor x t shape [] sentence
    cont <- convertSentences ret cont
    return $ Y.Let x t e cont
  (X.Declare {} : _) -> Left "Internal Error: unsupported form of declare found"
  (X.Assign {} : _) -> Left "Internal Error: unsupported form of assignment found"
  (X.Define x t e : cont) -> Y.Let x <$> convertType t <*> convertExpr e <*> convertSentences ret cont
  (X.Assert _ : cont) -> convertSentences ret cont -- ignore assert
  [X.Return e] -> convertExpr e
  (X.Return _ : _) -> Left "Syntax Error: dead code after return"

convertToplevelDecls :: Maybe VarName -> [X.ToplevelDecl] -> Either String Y.ToplevelExpr
convertToplevelDecls last = \case
  [] -> case last of
    Nothing -> Left "Syntax Error: empty program"
    Just x -> return $ Y.ResultExpr (Y.Var x)
  (X.ConstDef x t e : cont) -> do
    t <- convertType t
    e <- convertExpr e
    cont <- convertToplevelDecls (Just x) cont
    return $ Y.ToplevelLet Y.NonRec x [] t e cont
  (X.FunDef f args ret body : cont) -> do
    let x = VarName (unFunName f)
    args <- forM args $ \(x, t) -> do
      t <- convertType t
      return (x, t)
    ret <- convertType ret
    body <- convertSentences ret body
    cont <- convertToplevelDecls (Just x) cont
    return $ Y.ToplevelLet Y.Rec x args ret body cont

run :: X.Program -> Either String Y.Program
run prog = do
  prog <- convertToplevelDecls Nothing (X.decls prog)
  Y.typecheckProgram' prog
