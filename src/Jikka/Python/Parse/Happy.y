{
{-# LANGUAGE FlexibleContexts #-}

-- vim: filetype=haskell

-- |
-- Module      : Jikka.Core.Parse.Happy
-- Description : parses the code of the standard Python with Happy.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Python.Parse.Happy (run) where

import Data.Functor (($>))
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Jikka.Common.Error
import Jikka.Common.Language.Name
import Jikka.Common.Location
import qualified Jikka.Common.Parse.ShuntingYard as ShuntingYard
import Jikka.Common.Parse.ShuntingYard (BinOpInfo(..), Fixity(..))
import Jikka.Python.Language.Expr
import qualified Jikka.Python.Parse.Token as L (Token(..))
}

%name runEither
%tokentype { WithLoc L.Token }
%monad { Either Error }
%error { happyErrorExpList }
%errorhandlertype explist

%token
    -- literals
    none            { WithLoc _ L.None }
    integer         { WithLoc _ (L.Int _) }
    boolean         { WithLoc _ (L.Bool _) }
    string          { WithLoc _ (L.String _) }
    bytes           { WithLoc _ (L.Bytes _) }
    float           { WithLoc _ (L.Float _) }
    imaginary       { WithLoc _ (L.Imaginary _) }

    -- keywords
    def             { WithLoc _ L.Def }
    if              { WithLoc _ L.If }
    elif            { WithLoc _ L.Elif }
    else            { WithLoc _ L.Else }
    for             { WithLoc _ L.For }
    in              { WithLoc _ L.In }
    assert          { WithLoc _ L.Assert }
    return          { WithLoc _ L.Return }

    -- punctuations
    '->'            { WithLoc _ L.Arrow }
    ':'             { WithLoc _ L.Colon }
    ';'             { WithLoc _ L.Semicolon }
    ','             { WithLoc _ L.Comma }
    '.'             { WithLoc _ L.Dot }
    '='             { WithLoc _ L.Equal }
    '_'             { WithLoc _ L.Underscore }

    -- parens
    '['             { WithLoc _ L.OpenBracket }
    '('             { WithLoc _ L.OpenParen }
    '{'             { WithLoc _ L.OpenBrace }
    ']'             { WithLoc _ L.CloseBracket }
    ')'             { WithLoc _ L.CloseParen }
    '}'             { WithLoc _ L.CloseBrace }

    -- TODO: remove
    int             { WithLoc _ (L.Ident "int") }
    bool            { WithLoc _ (L.Ident "bool") }
    list            { WithLoc _ (L.Ident "List") }
    range           { WithLoc _ (L.Ident "range") }

    -- identifier
    ident           { WithLoc _ (L.Ident _) }

    -- operator
    walrus          { WithLoc _ L.WalrusOp }
    implies         { WithLoc _ L.ImpliesOp }
    or              { WithLoc _ L.OrOp }
    and             { WithLoc _ L.AndOp }
    not             { WithLoc _ L.NotOp }
    comp_operator   { WithLoc _ (L.CmpOp _) }
    '<?'            { WithLoc _ L.MinOp }
    '>?'            { WithLoc _ L.MaxOp }
    '|'             { WithLoc _ L.BitOrOp }
    '^'             { WithLoc _ L.BitXorOp }
    '&'             { WithLoc _ L.BitAndOp }
    '<<'            { WithLoc _ L.BitLShiftOp }
    '>>'            { WithLoc _ L.BitRShiftOp }
    '+'             { WithLoc _ L.PlusOp }
    '-'             { WithLoc _ L.MinusOp }
    '*'             { WithLoc _ L.MulOp }
    divmod_operator { WithLoc _ (L.DivModOp _) }
    '~'             { WithLoc _ L.BitNotOp }
    '**'            { WithLoc _ L.PowOp }

    -- indent
    newline         { WithLoc _ L.Newline }
    indent          { WithLoc _ L.Indent }
    dedent          { WithLoc _ L.Dedent }

    -- reserved
    as              { WithLoc _ L.As }
    async           { WithLoc _ L.Async }
    await           { WithLoc _ L.Await }
    break           { WithLoc _ L.Break }
    class           { WithLoc _ L.Class }
    continue        { WithLoc _ L.Continue }
    del             { WithLoc _ L.Del }
    except          { WithLoc _ L.Except }
    finally         { WithLoc _ L.Finally }
    from            { WithLoc _ L.From }
    global          { WithLoc _ L.Global }
    import          { WithLoc _ L.Import }
    is              { WithLoc _ L.Is }
    nonlocal        { WithLoc _ L.Nonlocal }
    pass            { WithLoc _ L.Pass }
    raise           { WithLoc _ L.Raise }
    try             { WithLoc _ L.Try }
    while           { WithLoc _ L.While }
    with            { WithLoc _ L.With }
    yield           { WithLoc _ L.Yield }
%%

Start :: { Program }
    : list(newline) list(ToplevelDecl)           { Program $2 }

-- utilities
opt(p) -- :: { Maybe a }
    : {- empty -}                      { Nothing }
    | p                                { Just p }
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
sep(p, q) -- :: { [a] }
    : {- empty -}                      { [] }
    | sep1(p, q)                       { $1 }

-- types
Type :: { Type' }
    : int                                        { $1 @> TyInt }
    | bool                                       { $1 @> TyBool }
    | list '[' Type ']'                          { $1 @> TyList $3 }
OptColonType :: { Maybe Type' }
    : {- empty -}                                { Nothing }
    | ':' Type                                   { Just $2 }
OptArrowType :: { Maybe Type' }
    : {- empty -}                                { Nothing }
    | '->' Type                                  { Just $2 }

-- literals
Literal :: { WithLoc Literal }
    : integer                          { let (L.Int n) = value $1 in $1 @> LitInt n }
    | boolean                          { let (L.Bool p) = value $1 in $1 @> LitBool p }

-- names
Name :: { WithLoc Name }
    : ident                            { let (L.Ident x) = value $1 in $1 @> x }
VarName :: { WithLoc VarName }
    : Name                             { VarName `fmap` $1 }
FunName :: { WithLoc FunName }
    : Name                             { FunName `fmap` $1 }
    | range                            { $1 @> FunName "range" }
VarNameOrUnderscore :: { WithLoc (Maybe VarName) }
    : VarName                          { Just `fmap` $1 }
    | '_'                              { $1 @> Nothing }

-- args
ActualArgs :: { [Expr'] }
    : sep(Expr, ',')                             { $1 }
FormalArg :: { (VarName, Maybe Type') }
    : VarName OptColonType                       { (value $1, $2) }
FormalArgs :: { [(VarName, Maybe Type')] }
    : sep(FormalArg, ',')                        { $1 }

-- lists
Comprehension :: { (Expr', WithLoc (Maybe VarName), Expr', Maybe Expr') }
    : Expr for VarNameOrUnderscore in Expr                 { ($1, $3, $5, Nothing) }
    | Expr for VarNameOrUnderscore in Expr if Expr         { ($1, $3, $5, Just $7) }
ListShape :: { WithLoc ListShape }
    : none                                                 { $1 @> NoneShape }
    | '[' ListShape for '_' in range '(' Expr ')' ']'      { $1 @> ListShape (value $2) $8 }
ListSub1 :: { [Expr'] }
    : '[' Expr ']'                     { [$2] }
    | '[' Expr ']' ListSub1            { $2 : $4 }

-- operators
BinaryOp :: { WithLoc FunName }
    : walrus                           { toFunName' $1 }
    | implies                          { toFunName' $1 }
    | or                               { toFunName' $1 }
    | and                              { toFunName' $1 }
    | not                              { toFunName' $1 }
    | comp_operator                    { toFunName' $1 }
    | '<?'                             { toFunName' $1 }
    | '>?'                             { toFunName' $1 }
    | '|'                              { toFunName' $1 }
    | '^'                              { toFunName' $1 }
    | '&'                              { toFunName' $1 }
    | '<<'                             { toFunName' $1 }
    | '>>'                             { toFunName' $1 }
    | '+'                              { toFunName' $1 }
    | '-'                              { toFunName' $1 }
    | '*'                              { toFunName' $1 }
    | divmod_operator                  { toFunName' $1 }
    | '~'                              { toFunName' $1 }
    | '**'                             { toFunName' $1 }

-- exprs
-- The operator precedence of Python is listed at https://docs.python.org/3/reference/expressions.html#operator-precedence
ExprAtom :: { Expr' }
    : VarName                                    { Var `fmap` $1 }
    | Literal                                    { Lit `fmap` $1 }
    | '[' ActualArgs ']'                         { $1 @> ListExt $2 }
    | '[' Comprehension ']'                      { let (body, var, iter, pred) = $2 in $1 @> ListComp (Comprehension body (value var) iter pred) }
    | VarName ListSub1                           { foldl (\a i -> $1 @> Sub a i) (Var `fmap` $1) $2 }
    | FunName '(' ActualArgs ')'                 { $1 @> Call (value $1) $3 }
    | FunName '(' Comprehension ')'              { let (body, var, iter, pred) = $3 in $1 @> Call (value $1) [$2 @> IterComp (Comprehension body (value var) iter pred)] }
    | '(' Expr ')'                               { $2 }
ExprPow :: { Expr' }
    : ExprAtom                                   { $1 }
    | ExprAtom '**' ExprPow                      { $1 @> Call (toFunName $2) [$1, $3] }
ExprUnOp :: { Expr' }
    : ExprPow                                    { $1 }
    | '-' ExprUnOp                               { $1 @> Call (toFunName $1) [$2] }
    | '~' ExprUnOp                               { $1 @> Call (toFunName $1) [$2] }
ExprBinOpList :: { (Expr', [(WithLoc FunName, Expr')]) }
    : ExprUnOp                                   { ($1, []) }
    | ExprBinOpList BinaryOp ExprUnOp            { let (x, ys) = $1 in (x, ys ++ [($2, $3)]) }
ExprBinOp :: { Expr' }
    : ExprBinOpList                              {% useShuntingYard $1 }
ExprNot :: { Expr' }
    : ExprBinOp                                  { $1 }
    | not ExprNot                                { $1 @> Call (toFunName $1) [$2] }
ExprAnd :: { Expr' }
    : ExprNot                                    { $1 }
    | ExprNot and ExprAnd                        { $1 @> Call (toFunName $2) [$1, $3] }
ExprOr :: { Expr' }
    : ExprAnd                                    { $1 }
    | ExprAnd or ExprOr                          { $1 @> Call (toFunName $2) [$1, $3] }
ExprImplies :: { Expr' }
    : ExprOr                                     { $1 }
    | ExprOr implies ExprImplies                 { $1 @> Call (toFunName $2) [$1, $3] }
Expr :: { Expr' }
    : ExprImplies                                { $1 }
    | Expr if ExprImplies else ExprImplies       { $1 @> Cond $3 $1 $5 }

-- simple statements
SimpleStatement :: { Sentence' }
    : VarName OptColonType '=' Expr              { $1 @> Define (value $1) $2 $4 }
    | VarName OptColonType '=' ListShape         { $1 @> Declare (value $1) $2 (value $4) }
    | VarName ListSub1 '=' Expr                  { $1 @> Assign (value $1) $2 $4 }
    | assert Expr                                { $1 @> Assert $2 }
    | return Expr                                { $1 @> Return $2 }

-- compound statements
IfStatementTail :: { [Sentence'] }
    : else ':' Suite                             { $3 }
    | elif Expr ':' Suite IfStatementTail        { [$1 @> If $2 $4 $5] }
CompoundStatement :: { Sentence' }
    : if Expr ':' Suite IfStatementTail          { $1 @> If $2 $4 $5 }
    | for VarName in Expr ':' Suite              { $1 @> For (value $2) $4 $6 }

-- statements
Suite :: { [Sentence'] }
    : list1(newline) indent list(newline) list1(Statement) dedent              { $4 }
Statement :: { Sentence' }
    : SimpleStatement list1(newline)                                           { $1 }
    | CompoundStatement list(newline)                                          { $1 }

-- toplevel declarations
ToplevelDecl :: { ToplevelDecl' }
    : from sep1(Name, '.') import '*' list1(newline)                           { $1 @> FromImport (map value $2) }
    | def FunName '(' FormalArgs ')' OptArrowType ':' Suite                    { $1 @> FunDef (value $2) $4 $6 $8 }
    | VarName OptColonType '=' Expr list1(newline)                             { $1 @> ConstDef (value $1) $2 $4 }

{
(@>) :: WithLoc a -> b -> WithLoc b
(@>) = ($>)

toFunName' :: WithLoc L.Token -> WithLoc FunName
toFunName' token = f <$> token where
    f L.PlusOp = FunName "-"
    f L.MinusOp = FunName "-"
    f L.MulOp = FunName "*"
    f L.PowOp = FunName "**"
    f _ = bug "invalid token as a function name"

toFunName :: WithLoc L.Token -> FunName
toFunName = value . toFunName'

builtInOps :: M.Map FunName BinOpInfo
builtInOps =
    let op fixity prec name = (FunName name, BinOpInfo fixity prec)
    in M.fromList
        [ op Leftfix 20 ">?"
        , op Leftfix 20 "<?"
        , op Leftfix 12 "*"
        , op Leftfix 12 "//"
        , op Leftfix 12 "%"
        , op Leftfix 12 "/^"
        , op Leftfix 11 "+"
        , op Leftfix 11 "-"
        , op Leftfix 10 "<<"
        , op Leftfix 10 ">>"
        , op Leftfix 9 "&"
        , op Leftfix 8 "^"
        , op Leftfix 7 "|"
        , op Nonfix 6 "=="
        , op Nonfix 6 "/="
        , op Nonfix 6 "<"
        , op Nonfix 6 "<="
        , op Nonfix 6 ">"
        , op Nonfix 6 ">="
        ]

useShuntingYard :: (Expr', [(WithLoc FunName, Expr')]) -> Either Error Expr'
useShuntingYard = ShuntingYard.run info apply where
    info :: FunName -> Either Error BinOpInfo
    info op = case M.lookup op builtInOps of
        Nothing -> Left (WithGroup InternalError (Error (show op ++ " is not defined")))
        Just op -> Right op
    apply :: WithLoc FunName -> Expr' -> Expr' -> Expr'
    apply op x y = x $> Call (value op) [x, y]

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
    case runEither tokens of
        Left err -> throwError err
        Right prog -> return prog
}
