module Jikka.Serializer.Python (run, run') where

import Data.List (intercalate, lookup)
import Data.Text (Text, pack)
import Jikka.Language.Common.Name
import Jikka.Language.Python.Typed.Stdlib
import Jikka.Language.Python.Typed.Type

indent :: String
indent = "<indent>"

dedent :: String
dedent = "<dedent>"

formatLiteral :: Literal -> String
formatLiteral (LitInt n) = show n
formatLiteral (LitBool p) = show p

formatUnaryOp :: UnaryOp -> Expr -> String
formatUnaryOp op e1 = show op ++ "(" ++ formatExpr e1 ++ ")"

formatBinaryOp :: BinaryOp -> Expr -> Expr -> String
formatBinaryOp op e1 e2 = show op ++ "(" ++ formatExpr e1 ++ ", " ++ formatExpr e2 ++ ")"

formatTernaryOp :: TernaryOp -> Expr -> Expr -> Expr -> String
formatTernaryOp op e1 e2 e3 = show op ++ "(" ++ formatExpr e1 ++ ", " ++ formatExpr e2 ++ ", " ++ formatExpr e3 ++ ")"

formatType :: Type -> String
formatType t = case t of
  ATyInt -> "int"
  ATyBool -> "bool"
  ATyList t -> "List[" ++ formatType t ++ "]"
  ATyNat -> "nat"
  ATyInterval l r -> "Interval[" ++ formatExpr l ++ ", " ++ formatExpr r ++ "]"
  ATyIterator t -> "Iterator[" ++ formatType t ++ "]"
  ATyArray t n -> "Array[" ++ formatType t ++ ", " ++ formatExpr n ++ "]"
  ATyVar name -> unTypeName name

formatComprehension :: Comprehension -> String
formatComprehension (Comprehension t1 e1 name t e2 e3) =
  let body = formatExpr e1 ++ " for " ++ unVarName name ++ " in " ++ formatExpr e2
      cond = case e3 of
        Nothing -> ""
        Just e3 -> " if " ++ formatExpr e3
   in body ++ cond

formatExpr :: Expr -> String
formatExpr e = case e of
  Var name -> unVarName name
  Lit lit -> formatLiteral lit
  UnOp op e1 -> formatUnaryOp op e1
  BinOp op e1 e2 -> formatBinaryOp op e1 e2
  TerOp op e1 e2 e3 -> formatTernaryOp op e1 e2 e3
  Sub _ (Var name) e2 -> unVarName name ++ "[" ++ formatExpr e2 ++ "]"
  Sub _ e1@Sub {} e2 -> formatExpr e1 ++ "[" ++ formatExpr e2 ++ "]"
  Sub _ e1 e2 -> "(" ++ formatExpr e1 ++ ")[" ++ formatExpr e2 ++ "]"
  ListExt _ es -> "[" ++ intercalate ", " (map formatExpr es) ++ "]"
  ListComp comp -> "[" ++ formatComprehension comp ++ "]"
  IterComp comp -> "(" ++ formatComprehension comp ++ ")"
  Call name [IterComp comp] -> unFunName name ++ "(" ++ formatComprehension comp ++ ")"
  Call name args -> unFunName name ++ "(" ++ intercalate ", " (map formatExpr args) ++ ")"

formatShape :: [Expr] -> String
formatShape [] = "None"
formatShape (e : es) = "[" ++ formatShape es ++ " for _ in range(" ++ formatExpr e ++ ")]"

formatSentence :: Sentence -> [String]
formatSentence sentence = case sentence of
  If e body1 body2 -> ["if " ++ formatExpr e ++ ":", indent] ++ concatMap formatSentence body1 ++ [dedent, "else:", indent] ++ concatMap formatSentence body2 ++ [dedent]
  For name t e body -> ["for " ++ unVarName name ++ " in " ++ formatExpr e ++ ":", indent] ++ concatMap formatSentence body ++ [dedent]
  Declare name t shape -> [unVarName name ++ ": " ++ formatType t ++ " = " ++ formatShape shape]
  Assign name indices e -> [unVarName name ++ concatMap (\e' -> "[" ++ formatExpr e' ++ "]") indices ++ " = " ++ formatExpr e]
  Define name t e -> [unVarName name ++ ": " ++ formatType t ++ " = " ++ formatExpr e]
  Assert e -> ["assert " ++ formatExpr e]
  Return e -> ["return " ++ formatExpr e]

formatToplevelDecl :: ToplevelDecl -> [String]
formatToplevelDecl decl = case decl of
  ConstDef name t e -> [unVarName name ++ ": " ++ formatType t ++ " = " ++ formatExpr e]
  FunDef name args ret body ->
    let args' = intercalate ", " $ map (\(name, t) -> unVarName name ++ ": " ++ formatType t) args
        def = "def " ++ unFunName name ++ "(" ++ args' ++ ") -> " ++ formatType ret ++ ":"
     in [def, indent] ++ concatMap formatSentence body ++ [dedent]

formatProgram :: Program -> [String]
formatProgram prog = concatMap formatToplevelDecl (decls prog)

makeIndent :: [String] -> [String]
makeIndent = go 0
  where
    go :: Int -> [String] -> [String]
    go _ [] = []
    go n (line : lines)
      | line == indent = go (n + 4) lines
      | line == dedent = go (n - 4) lines
      | otherwise = (replicate n ' ' ++ line) : go n lines

run' :: Program -> String
run' = unlines . makeIndent . formatProgram

run :: Program -> Either String Text
run = Right . pack . run'
