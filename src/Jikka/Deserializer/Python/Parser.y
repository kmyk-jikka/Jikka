{
-- vim: filetype=haskell
module Jikka.Deserializer.Python.Parser (run) where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Jikka.Deserializer.Python.Lexer as L (Token(..))
import qualified Jikka.Deserializer.ShuntingYard as ShuntingYard
import Jikka.Deserializer.ShuntingYard (BinOpInfo(..), Fixity(..))
import Jikka.Language.Common.Name
import Jikka.Language.Common.Pos
import Jikka.Language.Python.Parsed.Type
}

%name run
%tokentype { WithPos L.Token }
%monad { Either String }
%error { happyErrorExpList }
%errorhandlertype explist

%token
    none            { WithPos _ L.None }
    integer         { WithPos _ (L.Int _) }
    boolean         { WithPos _ (L.Bool _) }

    def             { WithPos _ L.Def }
    if              { WithPos _ L.If }
    elif            { WithPos _ L.Elif }
    else            { WithPos _ L.Else }
    for             { WithPos _ L.For }
    in              { WithPos _ L.In }
    assert          { WithPos _ L.Assert }
    return          { WithPos _ L.Return }
    import          { WithPos _ L.Import }
    from            { WithPos _ L.From }

    '->'            { WithPos _ L.Arrow }
    ':'             { WithPos _ L.Colon }
    ','             { WithPos _ L.Comma }
    '.'             { WithPos _ L.Dot }
    '='             { WithPos _ L.Equal }
    '_'             { WithPos _ L.Underscore }

    '['             { WithPos _ L.OpenBracket }
    '('             { WithPos _ L.OpenParen }
    ']'             { WithPos _ L.CloseBracket }
    ')'             { WithPos _ L.CloseParen }
    '\''            { WithPos _ L.SingleQuote }
    '"'             { WithPos _ L.DoubleQuote }

    int             { WithPos _ (L.Ident "int") }
    nat             { WithPos _ (L.Ident "nat") }
    bool            { WithPos _ (L.Ident "bool") }
    interval        { WithPos _ (L.Ident "Interval") }
    list            { WithPos _ (L.Ident "List") }
    array           { WithPos _ (L.Ident "Array") }
    range           { WithPos _ (L.Ident "range") }
    not             { WithPos _ (L.Ident "not") }
    and             { WithPos _ (L.Ident "and") }
    or              { WithPos _ (L.Ident "or") }
    implies         { WithPos _ (L.Ident "implies") }
    ident           { WithPos _ (L.Ident _) }

    '+'             { WithPos _ (L.Op "+") }
    '-'             { WithPos _ (L.Op "-") }
    '~'             { WithPos _ (L.Op "-") }
    '*'             { WithPos _ (L.Op "*") }
    '**'            { WithPos _ (L.Op "**") }
    op              { WithPos _ (L.Op _) }

    newline         { WithPos _ L.Newline }
    indent          { WithPos _ L.Indent }
    dedent          { WithPos _ L.Dedent }

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
    : int                                        { withPos $1 TyInt }
    | nat                                        { withPos $1 TyNat }
    | interval '[' QuotedExpr ',' QuotedExpr ']' { withPos $1 $ TyInterval $3 $5 }
    | bool                                       { withPos $1 TyBool }
    | list '[' Type ']'                          { withPos $1 $ TyList $3 }
    | array '[' Type ',' QuotedExpr ']'          { withPos $1 $ TyArray $3 $5 }
QuotedExpr :: { Expr' }
    : '"' Expr '"'                               { $2 }
    | '\'' Expr '\''                             { $2 }
OptColonType :: { Maybe Type' }
    : {- empty -}                                { Nothing }
    | ':' Type                                   { Just $2 }
OptArrowType :: { Maybe Type' }
    : {- empty -}                                { Nothing }
    | '->' Type                                  { Just $2 }

-- literals
Literal :: { WithPos Literal }
    : integer                          { let (L.Int n) = value $1 in withPos $1 $ LitInt n }
    | boolean                          { let (L.Bool p) = value $1 in withPos $1 $ LitBool p }

-- names
Name :: { WithPos Name }
    : ident                            { let (L.Ident x) = value $1 in withPos $1 x }
VarName :: { WithPos VarName }
    : Name                             { VarName `fmap` $1 }
FunName :: { WithPos FunName }
    : Name                             { FunName `fmap` $1 }
VarNameOrUnderscore :: { WithPos (Maybe VarName) }
    : VarName                          { Just `fmap` $1 }
    | '_'                              { withPos $1 $ Nothing }

-- args
ActualArgs :: { [Expr'] }
    : sep(Expr, ',')                             { $1 }
FormalArg :: { (VarName, Maybe Type') }
    : VarName OptColonType                       { (value $1, $2) }
FormalArgs :: { [(VarName, Maybe Type')] }
    : sep(FormalArg, ',')                        { $1 }

-- lists
Comprehension :: { (Expr', WithPos (Maybe VarName), Expr', Maybe Expr') }
    : Expr for VarNameOrUnderscore in Expr                 { ($1, $3, $5, Nothing) }
    | Expr for VarNameOrUnderscore in Expr if Expr         { ($1, $3, $5, Just $7) }
ListShape :: { WithPos ListShape }
    : none                                                 { withPos $1 NoneShape }
    | '[' ListShape for '_' in range '(' Expr ')' ']'      { withPos $1 $ ListShape (value $2) $8 }
ListSub1 :: { [Expr'] }
    : '[' Expr ']'                     { [$2] }
    | '[' Expr ']' ListSub1            { $2 : $4 }

-- operators
BinaryOp :: { WithPos FunName }
    : op                               { toFunName' $1 }
    | '+'                              { toFunName' $1 }
    | '-'                              { toFunName' $1 }
    | '*'                              { toFunName' $1 }

-- exprs
-- The operator precedence of Python is listed at https://docs.python.org/3/reference/expressions.html#operator-precedence
ExprAtom :: { Expr' }
    : VarName                                    { Var `fmap` $1 }
    | Literal                                    { Lit `fmap` $1 }
    | '[' ActualArgs ']'                         { withPos $1 $ ListExt $2 }
    | '[' Comprehension ']'                      { let (body, var, iter, pred) = $2 in withPos $1 $ ListComp body (value var) iter pred }
    | VarName ListSub1                           { foldl (\a i -> withPos $1 $ Sub a i) (Var `fmap` $1) $2 }
    | FunName '(' ActualArgs ')'                 { withPos $1 $ Call (value $1) $3 }
    | FunName '(' Comprehension ')'              { let (body, var, iter, pred) = $3 in withPos $1 $ Call (value $1) [withPos $2 $ ListComp body (value var) iter pred] }
    | '(' Expr ')'                               { $2 }
ExprPow :: { Expr' }
    : ExprAtom                                   { $1 }
    | ExprAtom '**' ExprPow                      { withPos $1 $ Call (toFunName $2) [$1, $3] }
ExprUnOp :: { Expr' }
    : ExprPow                                    { $1 }
    | '+' ExprUnOp                               { withPos $1 $ Call (toFunName $1) [$2] }
    | '-' ExprUnOp                               { withPos $1 $ Call (toFunName $1) [$2] }
    | '~' ExprUnOp                               { withPos $1 $ Call (toFunName $1) [$2] }
ExprBinOpList :: { (Expr', [(WithPos FunName, Expr')]) }
    : ExprUnOp                                   { ($1, []) }
    | ExprBinOpList BinaryOp ExprUnOp            { let (x, ys) = $1 in (x, ys ++ [($2, $3)]) }
ExprBinOp :: { Expr' }
    : ExprBinOpList                              {% useShuntingYard $1 }
ExprNot :: { Expr' }
    : ExprBinOp                                  { $1 }
    | not ExprNot                                { withPos $1 $ Call (toFunName $1) [$2] }
ExprAnd :: { Expr' }
    : ExprNot                                    { $1 }
    | ExprNot and ExprAnd                        { withPos $1 $ Call (toFunName $2) [$1, $3] }
ExprOr :: { Expr' }
    : ExprAnd                                    { $1 }
    | ExprAnd or ExprOr                          { withPos $1 $ Call (toFunName $2) [$1, $3] }
ExprImplies :: { Expr' }
    : ExprOr                                     { $1 }
    | ExprOr implies ExprImplies                 { withPos $1 $ Call (toFunName $2) [$1, $3] }
Expr :: { Expr' }
    : ExprImplies                                { $1 }
    | Expr if ExprImplies else ExprImplies       { withPos $1 $ Cond $3 $1 $5 }

-- simple statements
SimpleStatement :: { Sentence' }
    : VarName OptColonType '=' Expr              { withPos $1 $ Define (value $1) $2 $4 }
    | VarName OptColonType '=' ListShape         { withPos $1 $ Declare (value $1) $2 (value $4) }
    | VarName ListSub1 '=' Expr                  { withPos $1 $ Assign (value $1) $2 $4 }
    | assert Expr                                { withPos $1 $ Assert $2 }
    | return Expr                                { withPos $1 $ Return $2 }

-- compound statements
IfStatementTail :: { [Sentence'] }
    : else ':' Suite                             { $3 }
    | elif Expr ':' Suite IfStatementTail        { [withPos $1 $ If $2 $4 $5] }
CompoundStatement :: { Sentence' }
    : if Expr ':' Suite IfStatementTail          { withPos $1 $ If $2 $4 $5 }
    | for VarName in Expr ':' Suite              { withPos $1 $ For (value $2) $4 $6 }

-- statements
Suite :: { [Sentence'] }
    : list1(newline) indent list(newline) list1(Statement) dedent              { $4 }
Statement :: { Sentence' }
    : SimpleStatement list1(newline)                                           { $1 }
    | CompoundStatement list(newline)                                          { $1 }

-- toplevel declarations
ToplevelDecl :: { ToplevelDecl' }
    : from sep1(Name, '.') import '*' list1(newline)                           { withPos $1 $ FromImport (map value $2) }
    | def FunName '(' FormalArgs ')' OptArrowType ':' Suite                    { withPos $1 $ FunDef (value $2) $4 $6 $8 }
    | VarName OptColonType '=' Expr list1(newline)                             { withPos $1 $ ConstDef (value $1) $2 $4 }

{
toFunName' :: WithPos L.Token -> WithPos FunName
toFunName' token = f <$> token where
    f (L.Ident name) = FunName name
    f (L.Op name) = FunName name
    f _ = error "invalid token as a function name"

toFunName :: WithPos L.Token -> FunName
toFunName = value . toFunName'

builtInOps :: M.Map FunName BinOpInfo
builtInOps =
    let op fixity prec name = (FunName name, BinOpInfo fixity prec)
    in M.fromList
        [ op Leftfix 20 ">?"
        , op Leftfix 20 "<?"
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

useShuntingYard :: (Expr', [(WithPos FunName, Expr')]) -> Either String Expr'
useShuntingYard = ShuntingYard.run info apply where
    info :: FunName -> Either String BinOpInfo
    info op = case M.lookup op builtInOps of
        Nothing -> Left $ show op ++ " is not defined"
        Just op -> Right op
    apply :: WithPos FunName -> Expr' -> Expr' -> Expr'
    apply op x y = withPos x $ Call (value op) [x, y]

happyErrorExpList :: ([WithPos L.Token], [String]) -> Either String a
happyErrorExpList (tokens, expected) = Left $ "Syntax error at " ++ pos' tokens ++ ": " ++ tok tokens ++ " is got, but " ++ exp expected ++ " expected" where
    pos' [] = "EOF"
    pos' (token : _) = prettyPos (pos token)
    tok [] = "EOF"
    tok (token : _) = wrap . show $ value token
    exp [] = "EOF is"
    exp [item] = wrap item ++ " is"
    exp items = intercalate ", " (map wrap $ init items) ++ ", or " ++ (wrap $ last items) ++ " are"
    wrap ('\'' : s) = '`' : s
    wrap s = "`" ++ s ++ "'"
}
