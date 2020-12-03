module Jikka.Serializer.CPlusPlus.Converter (run) where

import qualified Jikka.Language.Parsed.Type as J
import Jikka.Serializer.CPlusPlus.Type

solveName :: Name
solveName = "solve"

mainName :: Name
mainName = "main"

convertHeaders :: J.Program -> [(IncludeType, Name)]
convertHeaders _ =
  [ (IncludeSystem, "cassert"),
    (IncludeSystem, "cstdint"),
    (IncludeSystem, "iostream"),
    (IncludeSystem, "vector")
  ]

convertType :: J.Type -> Either String Type
convertType t = case t of
  J.TyVar x -> Left $ "unexpected type variable: " ++ show x
  J.TyUnit -> Right TyVoid
  J.TyBool -> Right TyBool
  J.TyInt -> Right TyInt64
  J.TyFun J.TyUnit t' -> convertType t'
  J.TyFun J.TyBool t' -> TyArray <$> convertType t' <*> return 2
  J.TyFun J.TyInt t' -> TyVector <$> convertType t'
  J.TyFun _ _ -> Left $ "unexpected function type: " ++ show t

convertArgs :: J.Program -> Either String [(Name, Type)]
convertArgs prog = concat <$> mapM convertType' (J.given prog)
  where
    convertType' (x, t) = do
      t <- convertType t
      return $ case t of
        TyVoid -> [(x, t)]
        _ -> []

convertLiteral :: J.Literal -> Literal
convertLiteral lit = case lit of
  J.Unit -> Int32 0
  J.Bool p -> Bool p
  J.Int n -> Int64 n

convertExpr :: [(Name, Type)] -> J.Expr -> Either String ([Sentence], Expr)
convertExpr env e = case e of
  J.Lit lit -> return ([], Lit (convertLiteral lit))
  J.Var x -> return ([], Var x)
  J.BuiltIn builtin -> error $ "not implemented: " ++ show e
  J.Let x t e1 e2 -> error $ "not implemented: " ++ show e
  J.Fun funtype patterns -> error $ "not implemented: " ++ show e
  J.App e1 e2 -> error $ "not implemented: " ++ show e

convertSolve :: J.Program -> Either String [Sentence]
convertSolve prog = do
  args <- convertArgs prog
  (sentences, value) <- convertExpr args (J.body prog)
  return $ sentences ++ [Return value]

convertMain :: J.Program -> Either String [Sentence]
convertMain prog = do
  args <- convertArgs prog
  return [Return (Lit (Int32 0))]

run :: J.Program -> Either String Program
run prog = do
  let headers = convertHeaders prog
  args <- convertArgs prog
  solve <- convertSolve prog
  main <- convertMain prog
  return
    Program
      { includes = headers,
        decls =
          [ FunDecl solveName args solve,
            FunDecl mainName [] main
          ]
      }
