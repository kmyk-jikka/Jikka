{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- vim: filetype=haskell

-- |
-- Module      : Jikka.CPlusPlus.Parse.Happy
-- Description : parses programs of C++ with Happy.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- The grammer comes from <https://www.nongnu.org/hcb/ Hyperlinked C++ BNF Grammar>.
module Jikka.CPlusPlus.Parse.Happy
    ( run
    ) where

import Data.List (intercalate)
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.Location
import Jikka.CPlusPlus.Language.BuiltinPatterns
import Jikka.CPlusPlus.Language.Expr
import Jikka.CPlusPlus.Language.Util
import qualified Jikka.CPlusPlus.Parse.Token as L
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
    CHAR            { WithLoc _ (L.Char _) }
    string_literal  { WithLoc _ (L.String _) }

    -- keywords https://en.cppreference.com/w/cpp/keyword
    "alignas"                   { WithLoc _ (L.Keyword L.Alignas) }
    "alignof"                   { WithLoc _ (L.Keyword L.Alignof) }
    "and"                       { WithLoc _ (L.Keyword L.And) }
    "and_eq"                    { WithLoc _ (L.Keyword L.AndEq) }
    "asm"                       { WithLoc _ (L.Keyword L.Asm) }
    "atomic_cancel"             { WithLoc _ (L.Keyword L.AtomicCancel) }
    "atomic_commit"             { WithLoc _ (L.Keyword L.AtomicCommit) }
    "atomic_noexcept"           { WithLoc _ (L.Keyword L.AtomicNoexcept) }
    "auto"                      { WithLoc _ (L.Keyword L.Auto) }
    "bitand"                    { WithLoc _ (L.Keyword L.Bitand) }
    "bitor"                     { WithLoc _ (L.Keyword L.Bitor) }
    "bool"                      { WithLoc _ (L.Keyword L.Bool') }
    "break"                     { WithLoc _ (L.Keyword L.Break) }
    "case"                      { WithLoc _ (L.Keyword L.Case) }
    "catch"                     { WithLoc _ (L.Keyword L.Catch) }
    "char"                      { WithLoc _ (L.Keyword L.Char') }
    "char8_t"                   { WithLoc _ (L.Keyword L.Char8T) }
    "char16_t"                  { WithLoc _ (L.Keyword L.Char16T) }
    "char32_t"                  { WithLoc _ (L.Keyword L.Char32T) }
    "class"                     { WithLoc _ (L.Keyword L.Class) }
    "compl"                     { WithLoc _ (L.Keyword L.Compl) }
    "concept"                   { WithLoc _ (L.Keyword L.Concept) }
    "const"                     { WithLoc _ (L.Keyword L.Const) }
    "consteval"                 { WithLoc _ (L.Keyword L.Consteval) }
    "constexpr"                 { WithLoc _ (L.Keyword L.Constexpr) }
    "constinit"                 { WithLoc _ (L.Keyword L.Constinit) }
    "const_cast"                { WithLoc _ (L.Keyword L.ConstCast) }
    "continue"                  { WithLoc _ (L.Keyword L.Continue) }
    "co_await"                  { WithLoc _ (L.Keyword L.CoAwait) }
    "co_return"                 { WithLoc _ (L.Keyword L.CoReturn) }
    "co_yield"                  { WithLoc _ (L.Keyword L.CoYield) }
    "decltype"                  { WithLoc _ (L.Keyword L.Decltype) }
    "default"                   { WithLoc _ (L.Keyword L.Default) }
    "delete"                    { WithLoc _ (L.Keyword L.Delete) }
    "do"                        { WithLoc _ (L.Keyword L.Do) }
    "double"                    { WithLoc _ (L.Keyword L.Double) }
    "dynamic_cast"              { WithLoc _ (L.Keyword L.DynamicCast) }
    "else"                      { WithLoc _ (L.Keyword L.Else) }
    "enum"                      { WithLoc _ (L.Keyword L.Enum) }
    "explicit"                  { WithLoc _ (L.Keyword L.Explicit) }
    "export"                    { WithLoc _ (L.Keyword L.Export) }
    "extern"                    { WithLoc _ (L.Keyword L.Extern) }
    "false"                     { WithLoc _ (L.Keyword L.False') }
    "float"                     { WithLoc _ (L.Keyword L.Float) }
    "for"                       { WithLoc _ (L.Keyword L.For) }
    "friend"                    { WithLoc _ (L.Keyword L.Friend) }
    "goto"                      { WithLoc _ (L.Keyword L.Goto) }
    "if"                        { WithLoc _ (L.Keyword L.If) }
    "inline"                    { WithLoc _ (L.Keyword L.Inline) }
    "int"                       { WithLoc _ (L.Keyword L.Int') }
    "long"                      { WithLoc _ (L.Keyword L.Long) }
    "mutable"                   { WithLoc _ (L.Keyword L.Mutable) }
    "namespace"                 { WithLoc _ (L.Keyword L.Namespace) }
    "new"                       { WithLoc _ (L.Keyword L.New) }
    "noexcept"                  { WithLoc _ (L.Keyword L.Noexcept) }
    "not"                       { WithLoc _ (L.Keyword L.Not) }
    "not_eq"                    { WithLoc _ (L.Keyword L.NotEq) }
    "nullptr"                   { WithLoc _ (L.Keyword L.Nullptr) }
    "operator"                  { WithLoc _ (L.Keyword L.Operator') }
    "or"                        { WithLoc _ (L.Keyword L.Or) }
    "or_eq"                     { WithLoc _ (L.Keyword L.OrEq) }
    "private"                   { WithLoc _ (L.Keyword L.Private) }
    "protected"                 { WithLoc _ (L.Keyword L.Protected) }
    "public"                    { WithLoc _ (L.Keyword L.Public) }
    "reflexpr"                  { WithLoc _ (L.Keyword L.Reflexpr) }
    "register"                  { WithLoc _ (L.Keyword L.Register) }
    "reinterpret_cast"          { WithLoc _ (L.Keyword L.ReinterpretCast) }
    "requires"                  { WithLoc _ (L.Keyword L.Requires) }
    "return"                    { WithLoc _ (L.Keyword L.Return) }
    "short"                     { WithLoc _ (L.Keyword L.Short) }
    "signed"                    { WithLoc _ (L.Keyword L.Signed) }
    "sizeof"                    { WithLoc _ (L.Keyword L.Sizeof) }
    "static"                    { WithLoc _ (L.Keyword L.Static) }
    "static_assert"             { WithLoc _ (L.Keyword L.StaticAssert) }
    "static_cast"               { WithLoc _ (L.Keyword L.StaticCast) }
    "struct"                    { WithLoc _ (L.Keyword L.Struct) }
    "switch"                    { WithLoc _ (L.Keyword L.Switch) }
    "synchronized"              { WithLoc _ (L.Keyword L.Synchronized) }
    "template"                  { WithLoc _ (L.Keyword L.Template) }
    "this"                      { WithLoc _ (L.Keyword L.This) }
    "thread_local"              { WithLoc _ (L.Keyword L.ThreadLocal) }
    "throw"                     { WithLoc _ (L.Keyword L.Throw) }
    "true"                      { WithLoc _ (L.Keyword L.True') }
    "try"                       { WithLoc _ (L.Keyword L.Try) }
    "typedef"                   { WithLoc _ (L.Keyword L.Typedef) }
    "typeid"                    { WithLoc _ (L.Keyword L.Typeid) }
    "typename"                  { WithLoc _ (L.Keyword L.Typename) }
    "union"                     { WithLoc _ (L.Keyword L.Union) }
    "unsigned"                  { WithLoc _ (L.Keyword L.Unsigned) }
    "using"                     { WithLoc _ (L.Keyword L.Using) }
    "virtual"                   { WithLoc _ (L.Keyword L.Virtual) }
    "void"                      { WithLoc _ (L.Keyword L.Void) }
    "volatile"                  { WithLoc _ (L.Keyword L.Volatile) }
    "wchar_t"                   { WithLoc _ (L.Keyword L.WcharT) }
    "while"                     { WithLoc _ (L.Keyword L.While) }
    "xor"                       { WithLoc _ (L.Keyword L.Xor) }
    "xor_eq"                    { WithLoc _ (L.Keyword L.XorEq) }

    -- punctuations
    "&"             { WithLoc L.Ampersand }
    "->"            { WithLoc L.Arrow }
    ":"             { WithLoc L.Colon }
    ","             { WithLoc L.Comma }
    "."             { WithLoc L.Dot }
    "::"            { WithLoc L.DoubleColon }
    "="             { WithLoc L.Equal }
    "?"             { WithLoc L.Question }

    -- parens
    "["             { WithLoc _ L.OpenBracket }
    "("             { WithLoc _ L.OpenParen }
    "{"             { WithLoc _ L.OpenBrace }
    "]"             { WithLoc _ L.CloseBracket }
    ")"             { WithLoc _ L.CloseParen }
    "}"             { WithLoc _ L.CloseBrace }

    -- identifiers
    IDENT           { WithLoc _ (L.Ident _) }

    -- arithmetic operators
    "+"             { WithLoc _ (L.Operator L.Plus) }
    "-"             { WithLoc _ (L.Operator L.Minus) }
    "*"             { WithLoc _ (L.Operator L.Mult) }
    "/"             { WithLoc _ (L.Operator L.Div) }
    "%"             { WithLoc _ (L.Operator L.Mod) }

    -- augumented arithmetic operators
    "+="            { WithLoc _ (L.Operator L.PlusAssign) }
    "-="            { WithLoc _ (L.Operator L.MinusAssign) }
    "*="            { WithLoc _ (L.Operator L.MultAssign) }
    "/="            { WithLoc _ (L.Operator L.DivAssign) }
    "%="            { WithLoc _ (L.Operator L.ModAssign) }

    -- boolean operators
    "!"             { WithLoc _ (L.Operator L.LogicalNot) }
    "&&"            { WithLoc _ (L.Operator L.LogicalAnd) }
    "||"            { WithLoc _ (L.Operator L.LogicalOr) }

    -- bit operators
    "~"             { WithLoc _ (L.Operator L.BitNot) }
    "&"             { WithLoc _ (L.Operator L.BitAnd) }
    "|"             { WithLoc _ (L.Operator L.BitOr) }
    "^"             { WithLoc _ (L.Operator L.BitXor) }
    "<<"            { WithLoc _ (L.Operator L.BitLShift) }
    ">>"            { WithLoc _ (L.Operator L.BitRShift) }

    -- augumented bit operators
    "&="            { WithLoc _ (L.Operator L.BitAndAssign) }
    "|="            { WithLoc _ (L.Operator L.BitOrAssign) }
    "^="            { WithLoc _ (L.Operator L.BitXorAssign) }
    "<<="           { WithLoc _ (L.Operator L.BitLShiftAssign) }
    ">>="           { WithLoc _ (L.Operator L.BitRShiftAssign) }

    -- comparators
    ">"             { WithLoc _ (L.Operator L.GreaterThan) }
    "<"             { WithLoc _ (L.Operator L.LessThan) }
    "<="            { WithLoc _ (L.Operator L.LessEqual) }
    ">="            { WithLoc _ (L.Operator L.GreaterEqual) }
    "=="            { WithLoc _ (L.Operator L.DoubleEqual) }
    "/="            { WithLoc _ (L.Operator L.NotEqual) }

    -- additional keywords
    "int8_t"        { WithLoc _ L.Int8T }
    "int16_t"       { WithLoc _ L.Int16T }
    "int32_t"       { WithLoc _ L.Int32T }
    "int64_t"       { WithLoc _ L.Int64T }
    "uint8_t"       { WithLoc _ L.UInt8T }
    "uint16_t"      { WithLoc _ L.UInt16T }
    "uint32_t"      { WithLoc _ L.UInt32T }
    "uint64_t"      { WithLoc _ L.UInt64T }

    -- REP macros
    "REP"           { WithLoc _ L.REP }
    "REP3"          { WithLoc _ L.REP3 }
    "REP_R"         { WithLoc _ L.REP_R }
    "REP3R"         { WithLoc _ L.REP3R }

    -- types in std::
    "std"               { WithLoc _ L.Std }
    "array"             { WithLoc _ L.Array }
    "deque"             { WithLoc _ L.Deque }
    "list"              { WithLoc _ L.List }
    "map"               { WithLoc _ L.Map }
    "pair"              { WithLoc _ L.Pair }
    "priority_queue"    { WithLoc _ L.PriorityQueue }
    "queue"             { WithLoc _ L.Queue }
    "set"               { WithLoc _ L.Set }
    "tuple"             { WithLoc _ L.Tuple }
    "unordered_map"     { WithLoc _ L.UnorderedMap }
    "unordered_set"     { WithLoc _ L.UnorderedSet }
    "vector"            { WithLoc _ L.Vector }

    -- types in atcoder::
    "atcoder"           { WithLoc _ L.AtCoder }
    "segtree"           { WithLoc _ L.Segtree }

    -- types in jikka::
    "jikka"             { WithLoc _ L.Jikka }
    "convex_hull_trick" { WithLoc _ L.ConvexHullTrick }
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

-- expr.prim.general
primary_expression :: { Expr }
    : literal                          { $1 }
    | "(" expression ")"               { $1 }
    | id_expression                    {}
    | lambda_expression                {}
id_expression :: { Expr }
    : unqualified_id                   {}
unqualified_id :: { Expr }
    : identifier                       {}
nested_name_specifier :: { String }
    : namespace_name "::"              { $1 }

-- expr.prim.lambda
lambda_expression :: { Expr }
    : lambda_introducer lambda_declarator compound_statement
lambda_introducer :: { () }
    "[" opt(lambda_capture) "]"                             { () }
lambda_capture :: { () }
    : capture_default                                       { () }
capture_default :: { () }
    : "&"                                                   { () }
    | "="                                                   { () }
lambda_declarator :: { ([(Type, VarName)], Type) }
 	: "(" parameter_declaration_clause ")" trailing_return_type    { ($2, $4) }


-- expr.post
postfix_expression :: { Expr }
    : primary_expression                                                       {}
    | postfix_expression "[" expression "]"                                    {}
    | postfix_expression "(" opt(expression_list) ")"                          {}
    | simple_type_specifier ( opt(expression_list) )                           {}
    | typename_specifier ( opt(expression_list) )                              {}
    | simple_type_specifier braced_init_list                                   {}
    | typename_specifier braced_init_list                                      {}
    | postfix_expression "." opt(template) id_expression                       {}
    | postfix_expression "++"                                                  {}
    | postfix_expression "--"                                                  {}
    | "static_cast" "<" type_id ">" "(" expression ")"                         {}
    | typeid "(" expression ")"                                                {}
    | typeid "(" type_id ")"                                                   {}
expression_list :: { Expr }
    : initializer_list                                                         {}

-- expr.unary
unary_expression :: { Expr }
    : postfix_expression                                                       {}
    | "++" cast_expression                                                     {}
    | "--" cast_expression                                                     {}
    | unary_operator cast_expression                                           {}

unary-operator :: { Expr }
    : "*"                                                   {}
    | "&"                                                   {}
    | "+"                                                   {}
    | "-"                                                   {}
    | "!"                                                   {}
    | "~"                                                   {}

-- expr.cast
cast_expression :: { Expr }
    : unary_expression                                                         {}
    | "(" type_id ")" cast_expression                                          {}

-- expr.mptr.oper
pm_expression :: { Expr }
    : cast_expression                                                          {}

-- expr.mul
multiplicative_expression :: { Expr }
    : pm_expression                                                            {}
    | multiplicative_expression "*" pm_expression                              {}
    | multiplicative_expression "/" pm_expression                              {}
    | multiplicative_expression "%" pm_expression                              {}

-- expr.add
additive_expression :: { Expr }
    : multiplicative_expression                                                {}
    | additive_expression "+" multiplicative_expression                        {}
    | additive_expression "-" multiplicative_expression                        {}

-- expr.shift
shift_expression :: { Expr }
    : additive_expression                                                      {}
    | shift_expression "<<" additive_expression                                {}
    | shift_expression ">>" additive_expression                                {}

-- expr.rel
relational_expression :: { Expr }
    : shift_expression                                                         {}
    | relational_expression "<" shift_expression                               {}
    | relational_expression ">" shift_expression                               {}
    | relational_expression "<=" shift_expression                              {}
    | relational_expression ">=" shift_expression                              {}

-- expr.eq
equality_expression :: { Expr }
    : relational_expression                                                    {}
    | equality_expression "==" relational_expression                           {}
    | equality_expression "!=" relational_expression                           {}

-- expr.bit.and
and_expression :: { Expr }
    : equality_expression                                                      {}
    | and_expression "&" equality_expression                                   {}

-- expr.xor
exclusive_or_expression :: { Expr }
    : and_expression                                                           {}
    | exclusive_or_expression "^" and_expression                               {}

-- expr.or
inclusive_or_expression :: { Expr }
    : exclusive_or_expression                                                  {}
    | inclusive_or_expression "|" exclusive_or_expression                      {}

-- expr.log.and
logical_and_expression :: { Expr }
    : inclusive_or_expression                                                  {}
    | logical_and_expression "&&" inclusive_or_expression                      {}
    | logical_and_expression "and" inclusive_or_expression                     {}

-- expr.log.or
logical_or_expression :: { Expr }
    : logical_and_expression                                                   {}
    | logical_or_expression "||" logical_and_expression                        {}
    | logical_or_expression "or" logical_and_expression                        {}


-- expr.cond
conditional_expression :: { Expr }
    : logical_or_expression                                                    {}
    | logical_or_expression "?" expression ":" assignment_expression           {}

-- expr.ass
assignment_expression :: { Expr }
    : conditional_expression                                                   { $1 }
    | logical_or_expression assignment_operator initializer_clause             {}
assignment_operator :: {}
    : "="                                                  {}
    | "*="                                                 {}
    | "/="                                                 {}
    | "%="                                                 {}
    | "+="                                                 {}
    | "-="                                                 {}
    | ">>="                                                {}
    | "<<="                                                {}
    | "&="                                                 {}
    | "^="                                                 {}
    | "|="                                                 {}

-- expr.comma
expression :: { Expr }
 	: assignment_expression                                 { $1 }

-- expr.const
constant_expression :: { Expr }
    : conditional_expression                                { $1 }

-- stmt.stmt
statement :: { Statement }
    : expression_statement                                  { $1 }
    | compound_statement                                    { $1 }
    | selection_statement                                   { $1 }
    | iteration_statement                                   { $1 }
    | jump_statement                                        { $1 }
    | declaration_statement                                 { $1 }

-- stmt.expr
expression_statement :: { Statement }
    : expression ";"                                        { ExprStatement $1 }

-- stmt.block
compound_statement :: { Statement }
    : "{" list(statement) "}"                               { Block $2 }

-- stmt.select
selection_statement :: { Statement }
    : "if" "(" condition ")" statement                      { If $3 (toStatements $5) Nothing }
    | "if" "(" condition ")" statement "else" statement     { If $3 (toStatements $5) (Just (toStatements $7)) }
condition :: { Expr }
    : expression                                            { $1 }

-- stmt.iter
iteration_statement :: { Statement }
    : "for" "(" for_init_statement conditionopt ";" expressionopt ")" statement    { For }
    | "for" "(" for_range_declaration ":" for_range_initializer ")" statement      { For }
    | "REP" "(" identifier "," expression ")" statement                            { For IntTy $3 (Lit (LitInt32 0)) (BinOp LessThan (Var $3) $5) (AssignIncr (LeftVar $3)) (toStatements $7) }
    | "REP3" "(" identifier "," expression "," expression ")" statement            { For IntTy $3 $5 (BinOp LessThan (Var $3) $7) (AssignIncr (LeftVar $3)) (toStatements $9) }
    | "REP_R" "("identifier "," expression ")" statement                           { For IntTy $3 (BinOp Minus $5 (Lit (LitInt32 1))) (BinOp GreaterEqual (Var $3) (Lit (LitInt32 0))) (AssignDecr (LeftVar $3)) (toStatements $7) }
    | "REP3R" "(" identifier "," expression "," expression ")" statement           { For IntTy $3 (BinOp Minus $7 (Lit (LitInt32 1))) (BinOp GreaterEqual (Var $3) $5) (AssignDecr (LeftVar $3)) (toStatements $9) }
for_init_statement :: { Statement }
    : expression_statement                                  { $1 }
    | simple_declaration                                    { $1 }
for_range_declaration :: { Statement }
    : type_specifier_seq declarator                         {}
for_range_initializer :: { Statement }
    : expression braced_init_list                           {}

-- stmt.jump
jump_statement :: { Statement }
    : "return" expression ";"                               { Return $2 }

-- stmt.dcl
declaration_statement :: { Statement }
    : simple_declaration                                    { $1 }

-- dcl.dcl
declaration :: { ToplevelStatement }
    : simple_declaration                                    {}
    | function_definition                                   {}
block_declaration :: { Statement }
    simple_declaration                                      {}
    using_declaration                                       {}
    using_directive                                         {}
    static_assert_declaration                               {}
simple_declaration :: { Statement }
    : opt(decl_specifier_seq) opt(init_declarator_list) ";"    {}
static_assert_declaration :: { ToplevelStatement }
    : "static_assert" "(" constant_expression "," string_literal ")" ";"    { StaticAssert $3 $5 }

-- dcl.spec
decl_specifier :: { DeclSpecifier }
    : storage_class_specifier                               { StorageSepcDeclSpec $1 }
    | type_specifier                                        { TypeSpecDeclSpec $1 }
    | function_specifier                                    { FuncSpecDeclSpec $1 }
    | "friend"                                              { FriendDeclSpec }
    | "typedef"                                             { TypedefDeclSpec }
    | "constexpr"                                           { ConstExperDeclSpec }
decl_specifier_seq :: { [DeclSpecifier] }
    : decl_specifier                                        { [$1] }
    | decl_specifier decl_specifier_seq                     { $1 : $2 }

-- dcl.stc
storage_class_specifier :: { StorageClassSpecifier }
    : "register"                                            { RegisterStorageSpec }
    | "static"                                              { StaticStorageSpec }
    | "thread_local"                                        { ThreadLocalStorageSpec }
    | "extern"                                              { ExternStorageSpec }
    | "mutable"                                             { MutableStorageSpec }


-- dcl.fct.spec
function_specifier :: { FunctionSpecifier }
    : "inline"                                              { InlineFuncSpec }
    | "virtual"                                             { VirtualFuncSpec }
    | "explicit"                                            { ExplicitFuncSpec }

-- dcl.typedef
typedef_name :: {}
    : "int8_t"                         {}
    | "int16_t"                        {}
    | "int32_t"                        {}
    | "int64_t"                        {}
    | "uint8_t"                        {}
    | "uint16_t"                       {}
    | "uint32_t"                       {}
    | "uint64_t"                       {}

-- dcl.type
type_specifier :: { TypeSpecifier }
    : trailing_type_specifier          { $1 }
trailing_type_specifier :: { TypeSpecifier }
    : simple_type_specifier            { $1 }
    | cv_qualifier                     { $1 }
type_specifier_seq :: { [TypeSpecifier] }
    : list1(type_specifier)            { $1 }

-- dct.type.simple
simple_type_specifier :: { TypeSpecifier }
    : opt("::") opt(nested_name_specifier) type_name    { TypeName $1 }
    | "char"                           { CharTySpec }
    | "char16_t"                       { Char16TySpec }
    | "char32_t"                       { Char32TySpec }
    | "wchar_t"                        { WcharTySpec }
    | "bool"                           { BoolTySpec }
    | "short"                          { ShortTySpec }
    | "int"                            { IntTySpec }
    | "long"                           { LongTySpec }
    | "signed"                         { SignedTySpec }
    | "unsigned"                       { UnsignedTySpec }
    | "float"                          { FloatTySpec }
    | "double"                         { DoubleTySpec }
    | "void"                           { VoidTySpec }
    | "auto"                           { AutoTySpec }
type_name :: {}
    : typedef_name                     {}
    | simple_template_id               {}

-- namespace.def
namespace_name :: { String }
    : original_namespace_name          { $1 }
original_namespace_name :: { String }
    : "std"                            { "std" }
    | "atcoder"                        { "atcoder" }
    | "jikka"                          { "jikka" }

-- namespace.udir
using-directive :: { (Bool, [String], String) }
    : "using" "namespace" opt("::") opt(nested_name_specifier) namespace_name ";"    {}

-- dcl.decl
cv_qualifier :: { CVQualifier }
    : "const"                          { ConstQualifier }
    | "volatile"                       { VolatileQualifier }


-- dcl.name
type_id :: {}
    : type_specifier_seq opt(abstract_declarator)    {}

-- dcl.fct

-- temp.names
simple_template_id :: {}
    : template_name "<" opt(template_argument_list) ">"    {}
template_name :: {}
    : "array"                          {}
    | "deque"                          {}
    | "list"                           {}
    | "map"                            {}
    | "pair"                           {}
    | "priority_queue"                 {}
    | "queue"                          {}
    | "set"                            {}
    | "tuple"                          {}
    | "unordered_map"                  {}
    | "unordered_set"                  {}
    | "vector"                         {}
    | "segtree"                        {}

template_argument :: {}
    : constant_expression              {}
    | type_id                          {}

{
(<@>) :: Functor f => (a -> b) -> f a -> f b
(<@>) = (<$>)

underscoreTy :: Type
underscoreTy = VarTy (TypeName "_")

makeTuple :: MonadError Error m => [Expr] -> Type -> m Expr
makeTuple es t = case t of
    TupleTy ts | length ts == length es -> return $ uncurryApp (Tuple' ts) es
    _ -> throwSyntaxError "Jikka.CPlusPlus.Parse.Happy.makeTuple: wrong type annotation for tuple"

makeProj :: MonadError Error m => Expr -> Integer -> Type -> m Expr
makeProj e n t = case t of
    TupleTy ts -> return $ Proj' ts n e
    _ -> throwSyntaxError "Jikka.CPlusPlus.Parse.Happy.makeTuple: wrong type annotation for proj"

replaceUnderscoresT :: MonadAlpha m => Type -> m Type
replaceUnderscoresT = mapSubTypesM go where
  go = \case
    VarTy (TypeName "_") -> genType
    t -> return t

replaceUnderscoresE :: MonadAlpha m => [(VarName, Type)] -> Expr -> m Expr
replaceUnderscoresE env = mapExprM go env where
  go _ = \case
    Var (VarName "_") -> Var <$> genVarName'
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

run :: (MonadAlpha m, MonadError Error m) => [WithLoc L.Token] -> m Program
run tokens = wrapError' "Jikka.CPlusPlus.Parse.Happy.runProgram" $ do
    prog <- liftEither $ runProgram_ tokens
    prog <- mapTypeProgramM replaceUnderscoresT prog
    mapExprProgramM replaceUnderscoresE prog
}
