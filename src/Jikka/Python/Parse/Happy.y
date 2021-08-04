{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- vim: filetype=haskell

-- |
-- Module      : Jikka.Python.Parse.Happy
-- Description : parses the code of the standard Python with Happy.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Python.Parse.Happy (run) where

import Control.Arrow (first)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as M
import Jikka.Common.Error
import Jikka.Common.Location
import Jikka.Python.Language.Expr
import qualified Jikka.Python.Parse.Token as L
}

%name runHappy
%tokentype { WithLoc L.Token }
%monad { Either Error }
%error { happyErrorExpList }
%errorhandlertype explist

%token
    -- literals
    "None"          { WithLoc _ L.None }
    INTEGER         { WithLoc _ (L.Int _) }
    BOOLEAN         { WithLoc _ (L.Bool _) }
    STRING          { WithLoc _ (L.String _) }
    BYTES           { WithLoc _ (L.Bytes _) }
    FLOAT           { WithLoc _ (L.Float _) }
    IMAGINARY       { WithLoc _ (L.Imaginary _) }

    -- keywords
    "def"           { WithLoc _ L.Def }
    "if"            { WithLoc _ L.If }
    "elif"          { WithLoc _ L.Elif }
    "else"          { WithLoc _ L.Else }
    "for"           { WithLoc _ L.For }
    "in"            { WithLoc _ L.In }
    "assert"        { WithLoc _ L.Assert }
    "return"        { WithLoc _ L.Return }
    "lambda"        { WithLoc _ L.Lambda }

    -- punctuations
    "->"            { WithLoc _ L.Arrow }
    ":"             { WithLoc _ L.Colon }
    ";"             { WithLoc _ L.Semicolon }
    ","             { WithLoc _ L.Comma }
    "."             { WithLoc _ L.Dot }
    "="             { WithLoc _ L.Equal }
    "_"             { WithLoc _ L.Underscore }

    -- parens
    "["             { WithLoc _ L.OpenBracket }
    "("             { WithLoc _ L.OpenParen }
    "{"             { WithLoc _ L.OpenBrace }
    "]"             { WithLoc _ L.CloseBracket }
    ")"             { WithLoc _ L.CloseParen }
    "}"             { WithLoc _ L.CloseBrace }

    -- identifier
    IDENT           { WithLoc _ (L.Ident _) }

    -- operator
    ":="            { WithLoc _ L.WalrusOp }
    "implies"       { WithLoc _ L.ImpliesOp }
    "or"            { WithLoc _ L.OrOp }
    "and"           { WithLoc _ L.AndOp }
    "not"           { WithLoc _ L.NotOp }
    COMP_OPERATOR   { WithLoc _ (L.CmpOp $$) }
    "<?"            { WithLoc _ L.MinOp }
    ">?"            { WithLoc _ L.MaxOp }
    "|"             { WithLoc _ L.BitOrOp }
    "^"             { WithLoc _ L.BitXorOp }
    "&"             { WithLoc _ L.BitAndOp }
    "<<"            { WithLoc _ L.BitLShiftOp }
    ">>"            { WithLoc _ L.BitRShiftOp }
    "+"             { WithLoc _ L.PlusOp }
    "-"             { WithLoc _ L.MinusOp }
    "*"             { WithLoc _ L.MulOp }
    DIVMOD_OPERATOR { WithLoc _ (L.DivModOp $$) }
    "~"             { WithLoc _ L.BitNotOp }
    "**"            { WithLoc _ L.PowOp }
    "@"             { WithLoc _ L.AtOp }
    AUGOP           { WithLoc _ (L.AugOp $$) }

    -- indent
    NEWLINE         { WithLoc _ L.Newline }
    INDENT          { WithLoc _ L.Indent }
    DEDENT          { WithLoc _ L.Dedent }

    -- reserved
    "as"            { WithLoc _ L.As }
    "async"         { WithLoc _ L.Async }
    "await"         { WithLoc _ L.Await }
    "break"         { WithLoc _ L.Break }
    "class"         { WithLoc _ L.Class }
    "continue"      { WithLoc _ L.Continue }
    "del"           { WithLoc _ L.Del }
    "except"        { WithLoc _ L.Except }
    "finally"       { WithLoc _ L.Finally }
    "from"          { WithLoc _ L.From }
    "global"        { WithLoc _ L.Global }
    "import"        { WithLoc _ L.Import }
    "is"            { WithLoc _ L.Is }
    "nonlocal"      { WithLoc _ L.Nonlocal }
    "pass"          { WithLoc _ L.Pass }
    "raise"         { WithLoc _ L.Raise }
    "try"           { WithLoc _ L.Try }
    "while"         { WithLoc _ L.While }
    "with"          { WithLoc _ L.With }
    "yield"         { WithLoc _ L.Yield }
%%

file_input :: { [Statement'] }
    : {- empty -}                      { [] }
    | file_input NEWLINE               { $1 }
    | file_input statement             { $1 ++ $2 }

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

-- 6.2 Atoms
atom :: { Expr' }
    : identifier                       { $1 @> Name $1 }
    | literal                          { Constant `fmap` $1 }
    | enclosure                        { $1 }
enclosure :: { Expr' }
    : parenth_form                     { $1 }
    | list_display                     { $1 }

-- 6.2.1 Identifiers
identifier :: { Ident' }
    : IDENT                            { let (L.Ident x) = value $1 in $1 @> Ident x }
    | "_"                              { $1 @> Ident "_" }

-- 6.2.2 Literals
literal :: { WithLoc Constant }
    : "None"                           { $1 @> ConstNone }
    | INTEGER                          { let (L.Int n)       = value $1 in $1 @> ConstInt n }
    | BOOLEAN                          { let (L.Bool p)      = value $1 in $1 @> ConstBool p }
    | STRING                           { let (L.String s)    = value $1 in $1 @> ConstString s }
    | BYTES                            { let (L.Bytes s)     = value $1 in $1 @> ConstBytes s }
    | FLOAT                            { let (L.Float x)     = value $1 in $1 @> ConstFloat x }
    | IMAGINARY                        { let (L.Imaginary y) = value $1 in $1 @> ConstImaginary y }

-- 6.2.3 Parenthesized forms
parenth_form :: { Expr' }
    : "(" ")"                              { $1 @> Tuple [] }
    | "(" expression_list ")"              { uncurry fromExprList $2 }

-- 6.2.4 Displays for lists, sets and dictionaries
comprehension :: { (Expr', [Comprehension]) }
    : expression comp_for                                   { ($1, $2) }
comp_for :: { [Comprehension] }
    : "for" identifier "in" implies_test opt(comp_if)       { [Comprehension ($2 @> Name $2) $4 $5] }
comp_if :: { Expr' }
    : "if" expression_nocond                                { $2 }

-- 6.2.5 List displays
list_display :: { Expr' }
    : "[" "]"                                               { $1 @> List [] }
    | "[" expression_list "]"                               { $1 @> List (fst $2) }
    | "[" comprehension "]"                                 { $1 @> uncurry ListComp $2 }

-- 6.2.9. Yield expressions
yield_expression :: { Expr' }
    : "yield" opt(expression_list)                          { $1 @> Yield (uncurry fromExprList `fmap` $2) }
    | "yield" "from" expression                             { $1 @> YieldFrom $3 }

-- 6.3 Primaries
primary :: { Expr' }
    : atom                                                  { $1 }
    | attributeref                                          { $1 }
    | subscription                                          { $1 }
    | slicing                                               { $1 }
    | call                                                  { $1 }

-- 6.3.1. Attribute references
attributeref :: { Expr' }
    : primary "." identifier                                { $1 @> Attribute $1 $3 }

-- 6.3.2. Subscriptions
subscription :: { Expr' }
    : primary "[" expression_list "]"                       { $1 @> Subscript $1 (uncurry fromExprList $3) }

-- 6.3.3. Slicings
slicing :: { Expr' }
    : primary "[" opt(expression) ":" opt(expression) opt(snd(":", expression)) "]"                      { $1 @> Subscript $1 ($2 @> Slice $3 $5 $6) }

-- 6.3.4. Calls
call :: { Expr' }
    : primary "(" ")"                                       { $1 @> Call $1 [] [] }
    | primary "(" starred_list ")"                          { $1 @> Call $1 (fst $3) [] }
    | primary "(" comprehension ")"                         { $1 @> Call $1 [$2 @> uncurry GeneratorExp $3] [] }

-- 6.5. The power operator
power :: { Expr' }
    : primary                                               { $1 }
    | primary "**" u_expr                                   { $1 @> BinOp $1 Pow $3 }

-- 6.6. Unary arithmetic and bitwise operations
u_expr :: { Expr' }
    : power                                                 { $1 }
    | "-" u_expr                                            { $1 @> UnaryOp USub $2 }
    | "+" u_expr                                            { $1 @> UnaryOp UAdd $2 }
    | "~" u_expr                                            { $1 @> UnaryOp Invert $2 }

-- 6.7. Binary arithmetic operations
m_expr :: { Expr' }
    : u_expr                                                { $1 }
    | m_expr "*" u_expr                                     { $1 @> BinOp $1 Mult $3 }
    | m_expr "@" u_expr                                     { $1 @> BinOp $1 MatMult $3 }
    | m_expr DIVMOD_OPERATOR u_expr                         { $1 @> BinOp $1 (fromDivModOp $2) $3 }
a_expr :: { Expr' }
    : m_expr                                                { $1 }
    | a_expr "+" m_expr                                     { $1 @> BinOp $1 Add $3 }
    | a_expr "-" m_expr                                     { $1 @> BinOp $1 Sub $3 }

-- 6.8. Shifting operations
shift_expr :: { Expr' }
    : a_expr                                                { $1 }
    | shift_expr "<<" a_expr                                { $1 @> BinOp $1 BitLShift $3 }
    | shift_expr ">>" a_expr                                { $1 @> BinOp $1 BitRShift $3 }

-- 6.9. Binary bitwise operations
and_expr :: { Expr' }
    : shift_expr                                            { $1 }
    | and_expr "&" shift_expr                               { $1 @> BinOp $1 BitAnd $3 }
xor_expr :: { Expr' }
    : and_expr                                              { $1 }
    | xor_expr "^" and_expr                                 { $1 @> BinOp $1 BitXor $3 }
or_expr :: { Expr' }
    : xor_expr                                              { $1 }
    | or_expr "|" xor_expr                                  { $1 @> BinOp $1 BitOr $3 }

-- Extra.1. Min and max operations
min_expr :: { Expr' }
    : or_expr                                               { $1 }
    | min_expr "<?" or_expr                                 { $1 @> BinOp $1 Min $3 }
    | min_expr ">?" or_expr                                 { $1 @> BinOp $1 Max $3 }

-- 6.10. Comparisons
comparison :: { (Expr', [(CmpOp, Expr')]) }
    : min_expr                                              { ($1, []) }
    | comparison comp_operator min_expr                     { let (e1, e2) = $1 in (e1, e2 ++ [($2, $3)]) }
comp_operator :: { CmpOp }
    : COMP_OPERATOR                                         { fromCmpOp $1 }
    | "is"                                                  { Is }
    | "is" "not"                                            { IsNot }
    | "in"                                                  { In }
    | "not" "in"                                            { NotIn }

-- 6.11. Boolean operations
not_test :: { Expr' }
    : comparison                                            { convertCompare $1 }
    | "not" not_test                                        { $1 @> UnaryOp Not $2 }
and_test :: { Expr' }
    : not_test                                              { $1 }
    | and_test "and" not_test                               { $1 @> BoolOp $1 And $3 }
or_test :: { Expr' }
    : and_test                                              { $1 }
    | or_test "or" and_test                                 { $1 @> BoolOp $1 Or $3 }

-- Extra.2. Implication operation
implies_test :: { Expr' }
    : or_test                                               { $1 }
    | or_test "implies" implies_test                        { $1 @> BoolOp $1 Implies $3 }

-- 6.13. Conditional expressions
conditional_expression :: { Expr' }
    : implies_test                                          { $1 }
    | implies_test "if" implies_test "else" expression      { $1 @> IfExp $3 $1 $5 }
expression :: { Expr' }
    : conditional_expression                                { $1 }
    | lambda_expr                                           { $1 }
expression_nocond :: { Expr' }
    : implies_test                                          { $1 }
    | lambda_expr_nocond                                    { $1 }

-- 6.14. Lambda
lambda_expr :: { Expr' }
    : "lambda" ":" expression                                         { $1 @> Lambda emptyArguments $3}
    | "lambda" sep1opt(identifier, ",") ":" expression                { $1 @> Lambda (convertArguments' (reverse $2)) $4}
lambda_expr_nocond :: { Expr' }
    : "lambda" ":" expression_nocond                                  { $1 @> Lambda emptyArguments $3}
    | "lambda" sep1opt(identifier, ",") ":" expression_nocond         { $1 @> Lambda (convertArguments' (reverse $2)) $4}

-- 6.15. Expression lists
expression_list :: { ([Expr'], Bool) }
    : expression opt(",")                                   { ([$1], isJust $2) }
    | expression "," expression_list                        { first ($1 :) $3 }
starred_list :: { ([Expr'], Bool) }
    : starred_item opt(",")                                 { ([$1], isJust $2) }
    | starred_item "," starred_list                         { first ($1 :) $3 }
starred_item :: { Expr' }
    : expression                                            { $1 }
    | "*" min_expr                                          { $1 @> Starred $2 }

-- 7. Simple statements
simple_stmt :: { Statement' }
    : expression_stmt                                       { $1 }
    | assert_stmt                                           { $1 }
    | assignment_stmt                                       { $1 }
    | augmented_assignment_stmt                             { $1 }
    | annotated_assignment_stmt                             { $1 }
    | pass_stmt                                             { $1 }
    | del_stmt                                              { $1 }
    | return_stmt                                           { $1 }
    | yield_stmt                                            { $1 }
    | raise_stmt                                            { $1 }
    | break_stmt                                            { $1 }
    | continue_stmt                                         { $1 }
    | import_stmt                                           { $1 }
    | global_stmt                                           { $1 }
    | nonlocal_stmt                                         { $1 }

-- 7.1. Expression statements
expression_stmt :: { Statement' }
    : expression                                            { $1 @> Expr' $1 }

-- 7.2. Assignment statements
assignment_stmt :: { Statement' }
    : expression_list "=" expression                        { convertAssign $1 $3 }

-- 7.2.1. Augmented assignment statements
augmented_assignment_stmt :: { Statement' }
    : augtarget AUGOP expression_list                       { $1 @> AugAssign $1 (fromAugOp $2) (uncurry fromExprList $3) }
augtarget :: { Target' }
    : identifier                                            { $1 @> Name $1 }
    | attributeref                                          { $1 }
    | subscription                                          { $1 }
    | slicing                                               { $1 }

-- 7.2.2. Annotated assignment statements
annotated_assignment_stmt :: { Statement' }
    : augtarget ":" expression opt(snd("=", expression))    { $1 @> AnnAssign $1 $3 $4  }

-- 7.3. The assert statement
assert_stmt :: { Statement' }
    : "assert" expression opt(snd(",", expression))         { $1 @> Assert $2 $3 }

-- 7.4. The pass statement
pass_stmt :: { Statement' }
    : "pass"                                                { $1 @> Pass }

-- 7.5. The del statement¶
del_stmt :: { Statement' }
    : "del" expression_list                                 { $1 @> Delete (fst $2) }

-- 7.6. The return statement
return_stmt :: { Statement' }
    : "return" opt(expression_list)                         { $1 @> Return (uncurry fromExprList `fmap` $2) }

-- 7.7. The yield statement
yield_stmt :: { Statement' }
    : yield_expression                                      { $1 @> Expr' $1 }

-- 7.8. The raise statement
raise_stmt :: { Statement' }
    : "raise" opt(expression)                               { $1 @> Raise $2 Nothing }
    | "raise" expression "from" expression                  { $1 @> Raise (Just $2) (Just $4) }

-- 7.9. The break statement
break_stmt :: { Statement' }
    : "break"                                               { $1 @> Break }

-- 7.10. The continue statement
continue_stmt :: { Statement' }
    : "continue"                                            { $1 @> Continue }

-- 7.11. The import statement
import_stmt :: { Statement' }
    : "import" module_ opt(snd("as", identifier)) list(both(snd(",", module_), opt(snd("as", identifier))))  { $1 @> Import [] }
    | "from" module_ "import" "*"                           { $1 @> ImportFrom [] [] }
module_ :: { [Ident'] }
    : sep1(identifier, ".")                                 { $1 }

-- 7.12. The global statement
global_stmt :: { Statement' }
    : "global" sep1(identifier, ",")                        { $1 @> Global $2 }

-- 7.13. The nonlocal statement
nonlocal_stmt :: { Statement' }
    : "nonlocal" sep1(identifier, ",")                      { $1 @> Nonlocal $2 }

-- 8. Compound statements
compound_stmt :: { Statement' }
    : if_stmt                                               { $1 }
    | while_stmt                                            { $1 }
    | for_stmt                                              { $1 }
    | funcdef                                               { $1 }
suite :: { [Statement'] }
    : stmt_list NEWLINE                                     { $1 }
    | NEWLINE INDENT list1(statement) DEDENT                { concat $3 }
statement :: { [Statement'] }
    : stmt_list NEWLINE                                     { $1 }
    | compound_stmt                                         { [$1] }
stmt_list :: { [Statement'] }
    : simple_stmt opt(";")                                  { [$1] }
    | simple_stmt ";" stmt_list                             { $1 : $3 }

-- 8.1. The if statement
if_stmt :: { Statement' }
    : "if" expression ":" suite list(both(both("elif", expression), snd(":", suite))) opt(snd("else", snd(":", suite)))                 { convertIfElse $1 $2 $4 $5 $6 }

-- 8.2. The while statement
while_stmt :: { Statement' }
   : "while" expression ":" suite opt(snd("else", snd(":", suite)))                 { $1 @> While $2 $4 (fromMaybe [] $5) }

-- 8.3. The for statement
for_stmt :: { Statement' }
    : "for" identifier "in" expression_list ":" suite opt(snd("else", snd(":", suite)))                               { $1 @> For ($2 @> Name $2) (uncurry fromExprList $4) $6 (fromMaybe [] $7) }

-- 8.6. Function definitions
funcdef :: { Statement' }
    : list(decorator) "def" funcname "(" opt(parameter_list) ")" opt(snd("->", expression)) ":" suite                 { $2 @> FunctionDef $3 (convertArguments $5) $9 $1 $7 }
decorator :: { Decorator' }
    : "@" expression NEWLINE                                { $2 }
parameter_list :: { [Arg] }
    : parameter opt(",")                                    { [$1] }
    | parameter "," parameter_list                          { $1 : $3 }
parameter :: { Arg }
    : identifier opt(snd(":", expression))                  { ($1, $2) }
funcname :: { Ident' }
    : identifier                                            { $1 }

{
(@>) :: WithLoc a -> b -> WithLoc b
(@>) = ($>)

fromExprList :: [Expr'] -> Bool -> Expr'
fromExprList [] _ = bug "empty list for fromExprList"
fromExprList [e] False = e
fromExprList es@(e : _) _ = e $> Tuple es

fromCmpOp :: L.CmpOp -> CmpOp
fromCmpOp = \case
    L.DoubleEqual -> Eq'
    L.NotEqual -> NotEq
    L.LessThan -> Lt
    L.LessEqual -> LtE
    L.GreaterThan -> Gt
    L.GreaterEqual -> GtE

fromDivModOp :: L.DivModOp -> Operator
fromDivModOp = \case
    L.Div -> Div
    L.FloorDiv -> FloorDiv
    L.FloorMod -> FloorMod
    L.CeilDiv -> CeilDiv
    L.CeilMod -> CeilMod

fromAugOp :: L.AugOp -> Operator
fromAugOp = \case
    L.AugAdd -> Add
    L.AugSub -> Sub
    L.AugMul -> Mult
    L.AugAt -> MatMult
    L.AugDiv -> Div
    L.AugFloorDiv -> FloorDiv
    L.AugFloorMod -> FloorMod
    L.AugCeilDiv -> CeilDiv
    L.AugCeilMod -> CeilMod
    L.AugPow -> Pow
    L.AugBitRShift -> BitRShift
    L.AugBitLShift -> BitLShift
    L.AugBitAnd -> BitAnd
    L.AugBitXor -> BitXor
    L.AugBitOr -> BitOr
    L.AugMin -> Min
    L.AugMax -> Max

convertArguments :: Maybe [Arg] -> Arguments
convertArguments args = emptyArguments { argsArgs = fromMaybe [] args }

convertArguments' :: [Ident'] -> Arguments
convertArguments' args = emptyArguments { argsArgs = map (\x -> (x, Nothing)) args }

convertIfElse :: WithLoc a -> Expr' -> [Statement'] -> [((WithLoc a, Expr'), [Statement'])] -> Maybe [Statement'] -> Statement'
convertIfElse head cond body elifs orelse = head @> If cond body cont where
  cont = case elifs of
    [] -> fromMaybe [] orelse
    ((head', cond'), body') : elifs -> [convertIfElse head' cond' body' elifs orelse]

convertCompare :: (Expr', [(CmpOp, Expr')]) -> Expr'
convertCompare (e, []) = e
convertCompare (e, ops) = e $> Compare e ops

convertAssign :: ([Expr'], Bool) -> Expr' -> Statement'
convertAssign ([], _) _ = bug "empty targets for convertAssign"
convertAssign (xs, comma) e = head xs $> Assign [fromExprList xs comma] e

happyErrorExpList :: ([WithLoc L.Token], [String]) -> Either Error a
happyErrorExpList (tokens, expected) = Left err where
    err :: Error
    err = WithGroup SyntaxError (withLocation tokens (Error msg))
    withLocation :: [WithLoc L.Token] -> Error -> Error
    withLocation [] err = err
    withLocation (token : _) err = WithLocation(loc token) err
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

run :: MonadError Error m => [WithLoc L.Token] -> m Program
run tokens = wrapError' "Jikka.Python.Parse.Happy.run failed" $ do
    case runHappy tokens of
        Left err -> throwError err
        Right stmts -> return stmts
}
