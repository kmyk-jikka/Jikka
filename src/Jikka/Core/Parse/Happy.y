{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

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
    , runRule
    ) where

import Data.List (intercalate)
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.Location
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.TypeCheck
import Jikka.Core.Language.Util
import qualified Jikka.Core.Parse.Token as L
}

%name runProgram_ program
%name runExpr_ expression
%name runType_ type
%name runRule_ rule
%tokentype { WithLoc L.Token }
%monad { Either Error }
%error { happyErrorExpList }
%errorhandlertype explist

%token
    -- literals
    INTEGER         { WithLoc _ (L.Int _) }
    BOOLEAN         { WithLoc _ (L.Bool _) }
    STRING          { WithLoc _ (L.String _) }

    -- keywords
    "let"           { WithLoc _ L.Let }
    "rec"           { WithLoc _ L.Rec }
    "in"            { WithLoc _ L.In }
    "fun"           { WithLoc _ L.Fun }
    "if"            { WithLoc _ L.If }
    "then"          { WithLoc _ L.Then }
    "else"          { WithLoc _ L.Else }
    "assert"        { WithLoc _ L.Assert }
    "forall"        { WithLoc _ L.Forall }

    -- punctuations
    "->"            { WithLoc _ L.Arrow }
    ":"             { WithLoc _ L.Colon }
    ","             { WithLoc _ L.Comma }
    "="             { WithLoc _ L.Equal }
    "_"             { WithLoc _ L.Underscore }
    "."             { WithLoc _ L.Dot }
    "<-"            { WithLoc _ L.BackArrow }
    "@"             { WithLoc _ L.At }

    -- parens
    "["             { WithLoc _ L.OpenBracket }
    "("             { WithLoc _ L.OpenParen }
    "]"             { WithLoc _ L.CloseBracket }
    ")"             { WithLoc _ L.CloseParen }

    -- types
    "int"           { WithLoc _ (L.Ident "int") }
    "bool"          { WithLoc _ (L.Ident "bool") }
    "list"          { WithLoc _ (L.Ident "list") }
    "unit"          { WithLoc _ (L.Ident "unit") }
    "one_tuple"     { WithLoc _ (L.Ident "one_tuple") }
    "convex_hull_trick" { WithLoc _ (L.Ident "convex_hull_trick") }
    "segment_tree"  { WithLoc _ (L.Ident "segment_tree") }
    "int_plus"      { WithLoc _ (L.Ident "int_plus") }
    "int_min"       { WithLoc _ (L.Ident "int_min") }
    "int_max"       { WithLoc _ (L.Ident "int_max") }

    -- builtins
    "nil"           { WithLoc _ (L.Ident "nil") }
    "abs"           { WithLoc _ (L.Ident "abs") }
    "gcd"           { WithLoc _ (L.Ident "gcd") }
    "lcm"           { WithLoc _ (L.Ident "lcm") }
    "max"           { WithLoc _ (L.Ident "max") }
    "min"           { WithLoc _ (L.Ident "min") }
    "not"           { WithLoc _ (L.Ident "not") }
    "implies"       { WithLoc _ (L.Ident "implies") }
    "iterate"       { WithLoc _ (L.Ident "iterate") }
    "matap"         { WithLoc _ (L.Ident "matap") }
    "matzero"       { WithLoc _ (L.Ident "matzero") }
    "matone"        { WithLoc _ (L.Ident "matone") }
    "matadd"        { WithLoc _ (L.Ident "matadd") }
    "matmul"        { WithLoc _ (L.Ident "matmul") }
    "matpow"        { WithLoc _ (L.Ident "matpow") }
    "vecfloormod"   { WithLoc _ (L.Ident "vecfloormod") }
    "matfloormod"   { WithLoc _ (L.Ident "matfloormod") }
    "modnegate"     { WithLoc _ (L.Ident "modnegate") }
    "modplus"       { WithLoc _ (L.Ident "modplus") }
    "modminus"      { WithLoc _ (L.Ident "modminus") }
    "modmult"       { WithLoc _ (L.Ident "modmult") }
    "modinv"        { WithLoc _ (L.Ident "modinv") }
    "modpow"        { WithLoc _ (L.Ident "modpow") }
    "modmatap"      { WithLoc _ (L.Ident "modmatap") }
    "modmatadd"     { WithLoc _ (L.Ident "modmatadd") }
    "modmatmul"     { WithLoc _ (L.Ident "modmatmul") }
    "modmatpow"     { WithLoc _ (L.Ident "modmatpow") }
    "cons"          { WithLoc _ (L.Ident "cons") }
    "snoc"          { WithLoc _ (L.Ident "snoc") }
    "foldl"         { WithLoc _ (L.Ident "foldl") }
    "scanl"         { WithLoc _ (L.Ident "scanl") }
    "build"         { WithLoc _ (L.Ident "build") }
    "len"           { WithLoc _ (L.Ident "len") }
    "map"           { WithLoc _ (L.Ident "map") }
    "filter"        { WithLoc _ (L.Ident "filter") }
    "elem"          { WithLoc _ (L.Ident "elem") }
    "sum"           { WithLoc _ (L.Ident "sum") }
    "product"       { WithLoc _ (L.Ident "product") }
    "modsum"        { WithLoc _ (L.Ident "modsum") }
    "modproduct"    { WithLoc _ (L.Ident "modproduct") }
    "minimum"       { WithLoc _ (L.Ident "minimum") }
    "maximum"       { WithLoc _ (L.Ident "maximum") }
    "argmin"        { WithLoc _ (L.Ident "argmin") }
    "argmax"        { WithLoc _ (L.Ident "argmax") }
    "gcds"          { WithLoc _ (L.Ident "gcds") }
    "lcms"          { WithLoc _ (L.Ident "lcms") }
    "all"           { WithLoc _ (L.Ident "all") }
    "any"           { WithLoc _ (L.Ident "any") }
    "sorted"        { WithLoc _ (L.Ident "sorted") }
    "reversed"      { WithLoc _ (L.Ident "reversed") }
    "range"         { WithLoc _ (L.Ident "range") }
    "range2"        { WithLoc _ (L.Ident "range2") }
    "range3"        { WithLoc _ (L.Ident "range3") }
    "fact"          { WithLoc _ (L.Ident "fact") }
    "choose"        { WithLoc _ (L.Ident "choose") }
    "permute"       { WithLoc _ (L.Ident "permute") }
    "multichoose"   { WithLoc _ (L.Ident "multichoose") }
    "cht_init"      { WithLoc _ (L.Ident "cht_init") }
    "cht_getmin"    { WithLoc _ (L.Ident "cht_getmin") }
    "cht_insert"    { WithLoc _ (L.Ident "cht_insert") }
    "segtree_init"  { WithLoc _ (L.Ident "segtree_init") }
    "segtree_getrange"    { WithLoc _ (L.Ident "segtree_getrange") }
    "segtree_setpoint"    { WithLoc _ (L.Ident "segtree_setpoint") }

    -- identifiers
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
    "&&"             { WithLoc _ (L.Operator L.And) }
    "||"             { WithLoc _ (L.Operator L.Or) }

    -- bit operators
    "~"             { WithLoc _ (L.Operator L.BitNot) }
    "&"             { WithLoc _ (L.Operator L.BitAnd) }
    "|"             { WithLoc _ (L.Operator L.BitOr) }
    "^"             { WithLoc _ (L.Operator L.BitXor) }
    "<<"            { WithLoc _ (L.Operator L.BitLShift) }
    ">>"            { WithLoc _ (L.Operator L.BitRShift) }

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

rule :: { (String, [(VarName, Type)], Expr, Expr) }
    : STRING expression "=" expression                            { let L.String name = value $1 in (name, [], $2, $4) }
    | STRING "forall" list1(arg) "." expression "=" expression    { let L.String name = value $1 in (name, $3, $5, $7) }

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
    : "let" identifier opt_colon_type "=" expression "in"                    { ToplevelLet $2 $3 $5 }
    | "let" "rec" identifier list(arg) opt_colon_type "=" expression "in"    { ToplevelLetRec $3 $4 $5 $7 }

-- Types
atom_type :: { Type }
    : IDENT                            { let (L.Ident x) = value $1 in VarTy (TypeName x) }
    | "int"                            { IntTy }
    | "bool"                           { BoolTy }
    | atom_type "list"                 { ListTy $1 }
    | "unit"                           { TupleTy [] }
    | atom_type "one_tuple"            { TupleTy [$1] }
    | datastructure                    { DataStructureTy $1 }
    | "(" type ")"                     { $2 }

tuple_type :: { Type }
    : atom_type                        { $1 }
    | atom_type "*" sep1(atom_type, "*")    { TupleTy ($1 : $3) }

type :: { Type }
    : tuple_type                       { $1 }
    | tuple_type "->" type             { FunTy $1 $3 }

opt_colon_type :: { Type }
    : {- empty -}                      { underscoreTy }
    | ":" type                         { $2 }

-- Data Structures
datastructure :: { DataStructure }
    : "convex_hull_trick"              { ConvexHullTrick }
    | "segment_tree" "<" semigroup ">" { SegmentTree $3 }

semigroup :: { Semigroup' }
    : "int_plus"                      { SemigroupIntPlus }
    | "int_min"                       { SemigroupIntMin }
    | "int_max"                       { SemigroupIntMax }

-- Arguments
arg :: { (VarName, Type) }
    : identifier                       { ($1, underscoreTy) }
    | "(" identifier ":" type ")"      { ($2, $4) }

-- Atoms
atom :: { Expr }
    : identifier                       { Var $1 }
    | literal                          { Lit $1 }
    | parenth_form                     { $1 }
    | builtin                          { Lit (uncurry LitBuiltin $1) }

identifier :: { VarName }
    : IDENT                            { let (L.Ident x) = value $1 in VarName x }
    | "_"                              { VarName "_" }

integer :: { Integer }
    : INTEGER                          { let (L.Int n) = value $1 in n }

literal :: { Literal }
    : integer                          { LitInt $1 }
    | BOOLEAN                          { let (L.Bool p) = value $1 in LitBool p }
    | "nil"                            { LitNil underscoreTy }
    | "nil" "@" atom_type              { LitNil $3 }

parenth_form :: { Expr }
    : "(" ")"                                               {% makeTuple [] UnitTy }
    | "(" ")" "@" atom_type                                 {% makeTuple [] $4 }
    | "(" expression ")"                                    { $2 }
    | "(" expression "," ")"                                {% makeTuple [$2] (TupleTy [underscoreTy]) }
    | "(" expression "," ")" "@" atom_type                  {% makeTuple [$2] $6 }
    | "(" expression "," expression_list ")"                {% makeTuple ($2 : $4) (TupleTy (replicate (length ($2 : $4)) underscoreTy)) }
    | "(" expression "," expression_list ")" "@" atom_type  {% makeTuple ($2 : $4) $7 }

builtin :: { (Builtin, [Type]) }
    : "abs"                            { (Abs, []) }
    | "gcd"                            { (Gcd, []) }
    | "lcm"                            { (Lcm, []) }
    | "min"                            { (Min2, [underscoreTy]) }
    | "min" "@" atom_type              { (Min2, [$3]) }
    | "max"                            { (Max2, [underscoreTy]) }
    | "max" "@" atom_type              { (Max2, [$3]) }
    | "not"                            { (Not, []) }
    | "implies"                        { (Implies, []) }
    | "iterate"                        { (Iterate, [underscoreTy]) }
    | "iterate" "@" atom_type          { (Iterate, [$3]) }
    | "matap" "@" integer "@" integer  { (MatAp $3 $5, []) }
    | "matzero" "@" integer "@" integer    { (MatZero $3 $5, []) }
    | "matone" "@" integer             { (MatOne $3, []) }
    | "matadd" "@" integer "@" integer { (MatAdd $3 $5, []) }
    | "matmul" "@" integer "@" integer "@" integer { (MatMul $3 $5 $7, []) }
    | "matpow" "@" integer             { (MatPow $3, []) }
    | "vecfloormod" "@" integer        { (VecFloorMod $3, []) }
    | "matfloormod" "@" integer "@" integer    { (MatFloorMod $3 $5, []) }
    | "modnegate"                      { (ModNegate, []) }
    | "modplus"                        { (ModPlus, []) }
    | "modminus"                       { (ModMinus, []) }
    | "modmult"                        { (ModMult, []) }
    | "modinv"                         { (ModInv, []) }
    | "modpow"                         { (ModPow, []) }
    | "modmatap" "@" integer "@" integer    { (ModMatAp $3 $5, []) }
    | "modmatadd" "@" integer "@" integer    { (ModMatAdd $3 $5, []) }
    | "modmatmul" "@" integer "@" integer "@" integer    { (ModMatMul $3 $5 $7, []) }
    | "modmatpow" "@" integer          { (ModMatPow $3, []) }
    | "cons"                           { (Cons, [underscoreTy]) }
    | "cons" "@" atom_type             { (Cons, [$3]) }
    | "snoc"                           { (Snoc, [underscoreTy]) }
    | "snoc" "@" atom_type             { (Snoc, [$3]) }
    | "foldl"                          { (Foldl, [underscoreTy, underscoreTy]) }
    | "foldl" "@" atom_type "@" atom_type    { (Foldl, [$3, $5]) }
    | "scanl"                          { (Scanl, [underscoreTy, underscoreTy]) }
    | "scanl" "@" atom_type "@" atom_type    { (Scanl, [$3, $5]) }
    | "build"                          { (Build, [underscoreTy]) }
    | "build" "@" atom_type            { (Build, [$3]) }
    | "len"                            { (Len, [underscoreTy]) }
    | "len" "@" atom_type              { (Len, [$3]) }
    | "map"                            { (Map, [underscoreTy, underscoreTy]) }
    | "map" "@" atom_type "@" atom_type    { (Map, [$3, $5]) }
    | "filter"                         { (Filter, [underscoreTy]) }
    | "filter" "@" atom_type           { (Filter, [$3]) }
    | "elem"                           { (Elem, [underscoreTy]) }
    | "elem" "@" atom_type             { (Elem, [$3]) }
    | "sum"                            { (Sum, []) }
    | "product"                        { (Product, []) }
    | "modsum"                         { (ModSum, []) }
    | "modproduct"                     { (ModProduct, []) }
    | "minimum"                        { (Min1, [underscoreTy]) }
    | "minimum" "@" atom_type          { (Min1, [$3]) }
    | "maximum"                        { (Max1, [underscoreTy]) }
    | "maximum" "@" atom_type          { (Max1, [$3]) }
    | "argmin"                         { (ArgMin, [underscoreTy]) }
    | "argmin" "@" atom_type           { (ArgMin, [$3]) }
    | "argmax"                         { (ArgMax, [underscoreTy]) }
    | "argmax" "@" atom_type           { (ArgMax, [$3]) }
    | "gcds"                           { (Gcd1, []) }
    | "lcms"                           { (Lcm1, []) }
    | "all"                            { (All, []) }
    | "any"                            { (Any, []) }
    | "sorted"                         { (Sorted, [underscoreTy]) }
    | "sorted" "@" atom_type           { (Sorted, [$3]) }
    | "reversed"                       { (Reversed, [underscoreTy]) }
    | "reversed" "@" atom_type         { (Reversed, [$3]) }
    | "range"                          { (Range1, []) }
    | "range2"                         { (Range2, []) }
    | "range3"                         { (Range3, []) }
    | "fact"                           { (Fact, []) }
    | "choose"                         { (Choose, []) }
    | "permute"                        { (Permute, []) }
    | "multichoose"                    { (MultiChoose, []) }
    | "cht_init"                       { (ConvexHullTrickInit, []) }
    | "cht_getmin"                     { (ConvexHullTrickGetMin, []) }
    | "cht_insert"                     { (ConvexHullTrickInsert, []) }
    | "segtree_init" semigroup         { (SegmentTreeInitList $2, []) }
    | "segtree_getrange" semigroup     { (SegmentTreeGetRange $2, []) }
    | "segtree_setpoint" semigroup     { (SegmentTreeSetPoint $2, []) }

-- Primaries
primary :: { Expr }
    : atom                                                  { $1 }
    | subscription                                          { $1 }

-- Subscriptions
subscription :: { Expr }
    : primary "[" expression "]"                            { At' underscoreTy $1 $3 }
    | primary "[" expression "]" "@" atom_type              { At' $6 $1 $3 }
    | primary "[" expression "<-" expression "]"            { SetAt' underscoreTy $1 $3 $5 }
    | primary "[" expression "<-" expression "]" "@" atom_type    { SetAt' $8 $1 $3 $5 }
    | primary "." integer                                   {% makeProj $1 $3 underscoreTy }
    | primary "." integer "@" atom_type                     {% makeProj $1 $3 $5 }

-- Function applications
funapp :: { Expr }
    : primary                                              { $1 }
    | funapp primary                                       { App $1 $2 }

-- The power operator
power :: { Expr }
    : funapp                                                { $1 }
    | funapp "**" u_expr                                    { Pow' $1 $3 }

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

-- Comparisons
comparison :: { Expr }
    : or_expr                                               { $1 }
    | comparison comp_operator or_expr                      { $2 underscoreTy $1 $3 }
    | comparison comp_operator "@" atom_type or_expr        { $2 $4 $1 $5 }
comp_operator :: { Type -> Expr -> Expr -> Expr }
    : "=="                                                  { Equal' }
    | "/="                                                  { NotEqual' }
    | "<"                                                   { LessThan' }
    | ">"                                                   { GreaterThan' }
    | "<="                                                  { LessEqual' }
    | ">="                                                  { GreaterEqual' }

-- Boolean operations
and_test :: { Expr }
    : comparison                                            { $1 }
    | and_test "&&" comparison                              { And' $1 $3 }
or_test :: { Expr }
    : and_test                                              { $1 }
    | or_test "||" and_test                                 { Or' $1 $3 }

-- Conditional expressions
conditional_expression :: { Expr }
    : "if" expression "then" expression "else" expression   { If' underscoreTy $2 $4 $6 }
    | "if" "@" atom_type expression "then" expression "else" expression    { If' $3 $4 $6 $8 }

-- Lambda
lambda_expr :: { Expr }
    : "fun" list1(arg) "->" expression                       { curryLam $2 $4 }

-- Let
let_expr :: { Expr }
    : "let" identifier opt_colon_type "=" expression "in" expression    { Let $2 $3 $5 $7 }

-- Assertion
assert_expr :: { Expr }
    : "assert" expression "->" expression                       { Assert $2 $4 }

expression_nolet :: { Expr }
    : or_test                                               { $1 }
    | conditional_expression                                { $1 }
    | lambda_expr                                           { $1 }
    | assert_expr                                           { $1 }
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

makeTuple :: MonadError Error m => [Expr] -> Type -> m Expr
makeTuple es t = case t of
    TupleTy ts | length ts == length es -> return $ uncurryApp (Tuple' ts) es
    _ -> throwSyntaxError "Jikka.Core.Parse.Happy.makeTuple: wrong type annotation for tuple"

makeProj :: MonadError Error m => Expr -> Integer -> Type -> m Expr
makeProj e n t = case t of
    t | t == underscoreTy -> return $ Proj' [] n e -- A projection from the empty tuple is fixed in Jikka.Core.Convert.TypeInfer.
    TupleTy ts -> return $ Proj' ts n e
    _ -> throwSyntaxError "Jikka.Core.Parse.Happy.makeTuple: wrong type annotation for a tuple projection"

replaceUnderscoresT :: MonadAlpha m => Type -> m Type
replaceUnderscoresT = mapSubTypesM go where
  go = \case
    VarTy (TypeName "_") -> genType
    t -> return t

replaceUnderscoresE :: (MonadAlpha m, MonadError Error m) => [(VarName, Type)] -> Expr -> m Expr
replaceUnderscoresE env = mapSubExprM go env where
  go _ = \case
    Var (VarName "_") -> Var <$> genVarName'
    e@(Proj' [] i (Var x)) -> case lookup x env of
      Just (TupleTy ts) -> return $ Proj' ts i (Var x) -- Fix types of projections if it's easily possible.
      _ -> return e -- Some cases are impossible. You need to use Jikka.Core.Convert.TypeInfer.
    e -> return e

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

runRule :: (MonadAlpha m, MonadError Error m) => [WithLoc L.Token] -> m (String, [(VarName, Type)], Expr, Expr)
runRule tokens = wrapError' "Jikka.Core.Parse.Happy.runRule" $ do
    (name, args, e1, e2) <- liftEither $ runRule_ tokens
    args <- mapM (\(x, t) -> (x,) <$> replaceUnderscoresT t) args
    e1 <- mapTypeExprM replaceUnderscoresT e1
    e2 <- mapTypeExprM replaceUnderscoresT e2
    -- Don't replace underscores in exprs
    return (name, args, e1, e2)

runType :: (MonadAlpha m, MonadError Error m) => [WithLoc L.Token] -> m Type
runType tokens = wrapError' "Jikka.Core.Parse.Happy.runType" $ do
    t <- liftEither $ runType_ tokens
    replaceUnderscoresT t

runExpr :: (MonadAlpha m, MonadError Error m) => [WithLoc L.Token] -> m Expr
runExpr tokens = wrapError' "Jikka.Core.Parse.Happy.runExpr" $ do
    e <- liftEither $ runExpr_ tokens
    mapTypeExprM replaceUnderscoresT e
    mapSubExprM replaceUnderscoresE [] e

runProgram :: (MonadAlpha m, MonadError Error m) => [WithLoc L.Token] -> m Program
runProgram tokens = wrapError' "Jikka.Core.Parse.Happy.runProgram" $ do
    prog <- liftEither $ runProgram_ tokens
    prog <- mapTypeProgramM replaceUnderscoresT prog
    mapExprProgramM (mapSubExprM replaceUnderscoresE) prog
}
