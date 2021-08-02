{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- vim: filetype=haskell

-- |
-- Module      : Jikka.Core.Parse.Happy
-- Description : parses the code of the standard Core with Happy.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- See also Haskell's <https://www.haskell.org/onlinereport/syntax-iso.html Syntax Reference>.
module Jikka.Core.Parse.Happy
    ( runProgram
    , runExpr
    , runType
    ) where

import Data.List (intercalate)
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.Location
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Util (curryLam, genType, mapSubTypesM, mapTypeExprM, mapTypeProgramM, uncurryApp)
import qualified Jikka.Core.Parse.Token as L
}

%name runProgram_ program
%name runExpr_ expression
%name runType_ type
%tokentype { WithLoc L.Token }
%monad { Either Error }
%error { happyErrorExpList }
%errorhandlertype explist

%token
    -- literals
    INTEGER         { WithLoc _ (L.Int _) }
    BOOLEAN         { WithLoc _ (L.Bool _) }

    -- keywords
    "let"           { WithLoc _ L.Let }
    "rec"           { WithLoc _ L.Rec }
    "in"            { WithLoc _ L.In }
    "fun"           { WithLoc _ L.Fun }
    "if"            { WithLoc _ L.If }
    "then"          { WithLoc _ L.Then }
    "else"          { WithLoc _ L.Else }

    -- punctuations
    "->"            { WithLoc _ L.Arrow }
    ":"             { WithLoc _ L.Colon }
    ","             { WithLoc _ L.Comma }
    "="             { WithLoc _ L.Equal }
    "_"             { WithLoc _ L.Underscore }
    "'"             { WithLoc _ L.SingleQuote }
    "<-"            { WithLoc _ L.BackArrow }

    -- parens
    "["             { WithLoc _ L.OpenBracket }
    "("             { WithLoc _ L.OpenParen }
    "]"             { WithLoc _ L.CloseBracket }
    ")"             { WithLoc _ L.CloseParen }

    -- identifier
    "int"           { WithLoc _ (L.Ident "int") }
    "bool"          { WithLoc _ (L.Ident "bool") }
    "list"          { WithLoc _ (L.Ident "list") }
    "unit"          { WithLoc _ (L.Ident "unit") }
    "convex_hull_trick" { WithLoc _ (L.Ident "convex_hull_trick") }
    "segment_tree"  { WithLoc _ (L.Ident "segment_tree") }
    "int.plus"      { WithLoc _ (L.Ident "int.plus") }
    "int.min"       { WithLoc _ (L.Ident "int.min") }
    "int.max"       { WithLoc _ (L.Ident "int.max") }
    IDENT           { WithLoc _ (L.Ident _) }

    -- arithmetic operators
    "+"             { WithLoc _ (L.Operator L.Plus) }
    "-"             { WithLoc _ (L.Operator L.Minus) }
    "*"             { WithLoc _ (L.Operator L.Mult) }
    "/"             { WithLoc _ (L.Operator L.FloorDiv) }
    "%"             { WithLoc _ (L.Operator L.FloorMod) }
    "/^"            { WithLoc _ (L.Operator L.CeilDiv) }
    "%^"            { WithLoc _ (L.Operator L.CeilMod) }
    "**"            { WithLoc _ (L.Operator L.Pow) }

    -- boolean operators
    "and"           { WithLoc _ (L.Operator L.And) }
    "or"            { WithLoc _ (L.Operator L.Or) }
    "not"           { WithLoc _ (L.Operator L.Not) }
    "implies"       { WithLoc _ (L.Operator L.Implies) }

    -- bit operators
    "~"             { WithLoc _ (L.Operator L.BitNot) }
    "&"             { WithLoc _ (L.Operator L.BitAnd) }
    "|"             { WithLoc _ (L.Operator L.BitOr) }
    "^"             { WithLoc _ (L.Operator L.BitXor) }
    "<<"            { WithLoc _ (L.Operator L.BitLShift) }
    ">>"            { WithLoc _ (L.Operator L.BitRShift) }

    -- min max operators
    "<?"            { WithLoc _ (L.Operator L.Min) }
    ">?"            { WithLoc _ (L.Operator L.Max) }

    -- comparators
    ">"             { WithLoc _ (L.Operator L.GreaterThan) }
    "<"             { WithLoc _ (L.Operator L.LessThan) }
    "<="            { WithLoc _ (L.Operator L.LessEqual) }
    ">="            { WithLoc _ (L.Operator L.GreaterEqual) }
    "=="            { WithLoc _ (L.Operator L.DoubleEqual) }
    "/="            { WithLoc _ (L.Operator L.NotEqual) }
%%

program :: { Program }
    : topdecls                         { $1 }

-- utilities
opt(p) -- :: { Maybe a }
    : {- empty -}                      { Nothing }
    | p                                { Just $1 }
rev_list1(p) -- :: { [a] }
    : p                                { [$1] }
    | rev_list1(p) p                   { $2 : $1 }
list1(p) -- :: { [a] }
    : rev_list1(p)                     { reverse $1 }
list(p) -- :: { [a] }
    : {- empty -}                      { [] }
    | list1(p)                         { $1 }
rev_sep1(p, q) -- :: { [a] }
    : p                                { [$1] }
    | rev_sep1(p, q) q p               { $3 : $1 }
sep1(p, q) -- :: { [a] }
    : rev_sep1(p, q)                   { reverse $1 }
sep1opt(p, q) -- :: { [a] }
    : rev_sep1(p, q) opt(q)            { reverse $1 }
fst(p, q)
    : p q                              { $1 }
snd(p, q)
    : p q                              { $2 }
both(p, q)
    : p q                              { ($1, $2) }

topdecls :: { ToplevelExpr }
    : expression_nolet                 { ResultExpr $1 }
    | topdecl topdecls                 { $1 $2 }

topdecl :: { ToplevelExpr -> ToplevelExpr }
    : "let" identifier ":" type "=" expression "in"                    { ToplevelLet $2 $4 $6 }
    | "let" "rec" identifier list(arg) ":" type "=" expression "in"    { ToplevelLetRec $3 $4 $6 $8 }

-- Types
atom_type :: { Type }
    : "'" IDENT                        { let (L.Ident x) = value $2 in VarTy (TypeName x) }
    | "int"                            { IntTy }
    | "bool"                           { BoolTy }
    | atom_type "list"                 { ListTy $1 }
    | "unit"                           { TupleTy [] }
    | datastructure                    { DataStructureTy $1 }

tuple_type :: { Type }
    : atom_type                        { $1 }
    | atom_type "*" sep1(atom_type, "*") { TupleTy ($1 : $3) }

type :: { Type }
    : tuple_type                       { $1 }
    | tuple_type "->" type             { FunTy $1 $3 }
    | "(" type ")"                     { $2 }

-- Data Structures
datastructure :: { DataStructure }
    : "convex_hull_trick"              { ConvexHullTrick }
    | "segment_tree" "<" semigroup ">" { SegmentTree $3 }

semigroup :: { Semigroup' }
    : "int.plus"                      { SemigroupIntPlus }
    | "int.min"                       { SemigroupIntMin }
    | "int.max"                       { SemigroupIntMax }

-- Arguments
arg :: { (VarName, Type) }
    : "(" identifier ":" type ")"      { ($2, $4) }

-- Atoms
atom :: { Expr }
    : identifier                       { Var $1 }
    | literal                          { Lit $1 }
    | parenth_form                     { $1 }

-- Identifiers
identifier :: { VarName }
    : IDENT                            { let (L.Ident x) = value $1 in VarName x }
    | "_"                              { VarName "_" }

-- Literals
literal :: { Literal }
    : INTEGER                          { let (L.Int n) = value $1 in LitInt n }
    | BOOLEAN                          { let (L.Bool p) = value $1 in LitBool p }

-- Parenthesized forms
parenth_form :: { Expr }
    : "(" ")"                                               { makeTuple [] }
    | "(" expression ")"                                    { $2 }
    | "(" expression "," ")"                                { makeTuple [$2] }
    | "(" expression "," expression_list ")"                { makeTuple ($2 : $4) }

-- Primaries
primary :: { Expr }
    : atom                                                  { $1 }
    | subscription                                          { $1 }
    | primary atom                                          { App $1 $2 }

-- Subscriptions
subscription :: { Expr }
    : primary "[" expression "]"                            { At' underscoreTy $1 $3 }
    | primary "[" expression "<-" expression "]"            { SetAt' underscoreTy $1 $3 $5 }

-- The power operator
power :: { Expr }
    : primary                                               { $1 }
    | primary "**" u_expr                                   { Pow' $1 $3 }

-- Unary arithmetic and bitwise operations
u_expr :: { Expr }
    : power                                                 { $1 }
    | "-" u_expr                                            { Negate' $2 }
    | "+" u_expr                                            { $2 }
    | "~" u_expr                                            { BitNot' $2 }

-- Binary arithmetic operations
m_expr :: { Expr }
    : u_expr                                                { $1 }
    | m_expr "*" u_expr                                     { Mult' $1 $3 }
    | m_expr "/" u_expr                                     { FloorDiv' $1 $3 }
    | m_expr "%" u_expr                                     { FloorMod' $1 $3 }
    | m_expr "/^" u_expr                                    { CeilDiv' $1 $3 }
    | m_expr "%^" u_expr                                    { CeilMod' $1 $3 }
a_expr :: { Expr }
    : m_expr                                                { $1 }
    | a_expr "+" m_expr                                     { Plus' $1 $3 }
    | a_expr "-" m_expr                                     { Minus' $1 $3 }

-- Shifting operations
shift_expr :: { Expr }
    : a_expr                                                { $1 }
    | shift_expr "<<" a_expr                                { BitLeftShift' $1 $3 }
    | shift_expr ">>" a_expr                                { BitRightShift' $1 $3 }

-- 6.9. Binary bitwise operations
and_expr :: { Expr }
    : shift_expr                                            { $1 }
    | and_expr "&" shift_expr                               { BitAnd' $1 $3 }
xor_expr :: { Expr }
    : and_expr                                              { $1 }
    | xor_expr "^" and_expr                                 { BitXor' $1 $3 }
or_expr :: { Expr }
    : xor_expr                                              { $1 }
    | or_expr "|" xor_expr                                  { BitOr' $1 $3 }

-- Min and max operations
min_expr :: { Expr }
    : or_expr                                               { $1 }
    | min_expr "<?" or_expr                                 { Min2' underscoreTy $1 $3 }
    | min_expr ">?" or_expr                                 { Max2' underscoreTy $1 $3 }

-- Comparisons
comparison :: { Expr }
    : min_expr                                              { $1 }
    | comparison comp_operator min_expr                     { $2 $1 $3 }
comp_operator :: { Expr -> Expr -> Expr }
    : "=="                                                  { Equal' underscoreTy }
    | "/="                                                  { NotEqual' underscoreTy }
    | "<"                                                   { LessThan' underscoreTy }
    | ">"                                                   { GreaterThan' underscoreTy }
    | "<="                                                  { LessEqual' underscoreTy }
    | ">="                                                  { GreaterEqual' underscoreTy }

-- Boolean operations
not_test :: { Expr }
    : comparison                                            { $1 }
    | "not" not_test                                        { Not' $2 }
and_test :: { Expr }
    : not_test                                              { $1 }
    | and_test "and" not_test                               { And' $1 $3 }
or_test :: { Expr }
    : and_test                                              { $1 }
    | or_test "or" and_test                                 { Or' $1 $3 }

-- Implication operation
implies_test :: { Expr }
    : or_test                                               { $1 }
    | or_test "implies" implies_test                        { Implies' $1 $3 }

-- Conditional expressions
conditional_expression :: { Expr }
    : "if" expression "then" expression "else" expression   { If' underscoreTy $2 $4 $6 }

-- Lambda
lambda_expr :: { Expr }
    : "fun" list1(arg) "->" expression                       { curryLam $2 $4 }

-- Let
let_expr :: { Expr }
    : "let" identifier ":" type "=" expression "in" expression    { Let $2 $4 $6 $8 }

expression_nolet :: { Expr }
    : implies_test                                          { $1 }
    | conditional_expression                                { $1 }
    | lambda_expr                                           { $1 }
expression :: { Expr }
    : expression_nolet                                      { $1 }
    | let_expr                                              { $1 }

-- Expression lists
expression_list :: { [Expr] }
    : sep1(expression, ",")                                 { $1 }

{
(<@>) :: Functor f => (a -> b) -> f a -> f b
(<@>) = (<$>)

underscoreTy :: Type
underscoreTy = VarTy (TypeName "_")

makeTuple :: [Expr] -> Expr
makeTuple es =
    let ts = replicate (length es) underscoreTy
    in uncurryApp (Tuple' ts) es

replaceUnderscores :: MonadAlpha m => Type -> m Type
replaceUnderscores = mapSubTypesM go where
  go = \case
    VarTy (TypeName "_") ->genType
    t -> return t

happyErrorExpList :: MonadError Error m => ([WithLoc L.Token], [String]) -> m a
happyErrorExpList (tokens, expected) = throwSyntaxErrorAt' loc' msg where
    loc' :: Maybe Loc
    loc' = case tokens of
        [] -> Nothing
        (token : _) -> Just (loc token)
    msg :: String
    msg = tok tokens ++ " is got, but " ++ exp expected ++ " expected"
    tok :: [WithLoc L.Token] -> String
    tok [] = "EOF"
    tok (token : _) = wrap . show $ value token
    exp :: [String] -> String
    exp [] = "EOF is"
    exp [item] = wrap item ++ " is"
    exp items = intercalate ", " (map wrap $ init items) ++ ", or " ++ (wrap $ last items) ++ " are"
    wrap :: String -> String
    wrap ('\'' : s) = '`' : s
    wrap s = "`" ++ s ++ "'"

runType :: (MonadAlpha m, MonadError Error m) => [WithLoc L.Token] -> m Type
runType tokens = wrapError' "Jikka.Core.Parse.Happy.runType" $ do
    t <- liftEither $ runType_ tokens
    replaceUnderscores t

runExpr :: (MonadAlpha m, MonadError Error m) => [WithLoc L.Token] -> m Expr
runExpr tokens = wrapError' "Jikka.Core.Parse.Happy.runExpr" $ do
    e <- liftEither $ runExpr_ tokens
    mapTypeExprM replaceUnderscores e

runProgram :: (MonadAlpha m, MonadError Error m) => [WithLoc L.Token] -> m Program
runProgram tokens = wrapError' "Jikka.Core.Parse.Happy.runProgram" $ do
    prog <- liftEither $ runProgram_ tokens
    mapTypeProgramM replaceUnderscores prog
}
