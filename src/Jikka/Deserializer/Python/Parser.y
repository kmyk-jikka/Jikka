{
-- vim: filetype=haskell
module Jikka.Deserializer.Python.Parser (run) where

import Data.List (intercalate)
import qualified Jikka.Deserializer.Python.Lexer as L (Token(..))
import Jikka.Deserializer.Pos
import qualified Jikka.Deserializer.ShuntingYard as ShuntingYard
import Jikka.Deserializer.ShuntingYard (BinOpInfo(..), Fixity(..))
import Jikka.Language.Python.Type
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

    '{'             { WithPos _ L.OpenBrace }
    '['             { WithPos _ L.OpenBracket }
    '('             { WithPos _ L.OpenParen }
    '}'             { WithPos _ L.CloseBrace }
    ']'             { WithPos _ L.CloseBracket }
    ')'             { WithPos _ L.CloseParen }

    int             { WithPos _ (L.Ident "int") }
    nat             { WithPos _ (L.Ident "nat") }
    bool            { WithPos _ (L.Ident "bool") }
    interval        { WithPos _ (L.Ident "Interval") }
    list            { WithPos _ (L.Ident "List") }
    array           { WithPos _ (L.Ident "Array") }
    range           { WithPos _ (L.Ident "range") }
    ident           { WithPos _ (L.Ident _) }

    '+'             { WithPos _ (L.Op "+") }
    '-'             { WithPos _ (L.Op "-") }
    '*'             { WithPos _ (L.Op "*") }
    op              { WithPos _ (L.Op _) }

    newline         { WithPos _ L.Newline }
    indent          { WithPos _ L.Indent }
    dedent          { WithPos _ L.Dedent }

%%

Start :: { Program }
    : list(newline) list(ToplevelDecl) list(newline)  { Program $2 }

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
Type :: { Type }
    : int                              { TyInt }
    | nat                              { TyNat }
    | interval '[' Expr ',' Expr ']'   { TyInterval $3 $5 }
    | bool                             { TyBool }
    | list '[' Type ']'                { TyList $3 }
    | array '[' Type ',' Expr ']'      { TyArray $3 $5 }

-- literals
IntLiteral :: { Integer }
    : integer                          { let (L.Int n) = value $1 in n }
BoolLiteral :: { Bool }
    : boolean                          { let (L.Bool p) = value $1 in p }
Literal :: { Literal }
    : IntLiteral                       { LitInt $1 }
    | BoolLiteral                      { LitBool $1 }

-- names
Name :: { Name }
    : ident                            { let (L.Ident x) = value $1 in x }
VarName :: { VarName }
    : Name                             { VarName $1 }
FunName :: { FunName }
    : Name                             { FunName $1 }
VarNameOrUnderscore :: { Maybe VarName }
    : VarName                          { Just $1 }
    | '_'                              { Nothing }

-- args
ActualArgs :: { [Expr] }
    : sep(Expr, ',')                   { $1 }
FormalArg :: { (VarName, Type) }
    : VarName ':' Type                 { ($1, $3) }
FormalArgs :: { [(VarName, Type)] }
    : sep(FormalArg, ',')             { $1 }

-- lists
Comprehension :: { (Expr, Maybe VarName, Expr, Maybe Expr) }
    : Expr for VarNameOrUnderscore in Expr         { ($1, $3, $5, Nothing) }
    | Expr for VarNameOrUnderscore in Expr if Expr { ($1, $3, $5, Just $7) }
ListShape :: { [Expr] }
    : '[' none for '_' in range '(' Expr ')' ']'  { [$8] }
    | '[' ListShape for '_' in range '(' Expr ')' ']'  { $8 : $2 }
ListSub :: { [Expr] }
    : '[' Expr ']'                     { [$2] }
    | '[' Expr ']' ListSub             { $2 : $4 }

-- exprs
Atom :: { Expr }
    : VarName                          { Var $1 }
    | Literal                          { Lit $1 }
    | '(' Expr ')'                     { $2 }
Expr :: { Expr }
    : Atom                             { $1 }
    | Expr '[' Expr ']'                { Sub $1 $3 }
    | '[' ActualArgs ']'               { ListExt $2 }
    | '[' Comprehension ']'            { let (body, var, iter, pred) = $2 in ListComp body var iter pred }
    | FunName '(' ActualArgs ')'       { Call $1 $3 }
    | FunName '(' Comprehension ')'    { let (body, var, iter, pred) = $3 in Call $1 [ListComp body var iter pred] }
    | Expr if Expr else Expr           { Cond $3 $1 $5 }

-- simple statements
SimpleStatement :: { Sentence }
    : VarName ':' Type '=' Expr        { Define $1 $3 $5 }
    | VarName ':' Type '=' ListShape   { Declare $1 $3 $5 }
    | VarName ListSub '=' Expr         { Assign $1 $2 $4 }
    | assert Expr                      { Assert $2 }
    | return Expr                      { Return $2 }

-- compound statements
IfStatementTail :: { [Sentence] }
    : else ':' Suite                   { $3 }
    | elif Expr ':' Suite IfStatementTail  { [If $2 $4 $5] }
CompoundStatement :: { Sentence }
    : if Expr ':' Suite IfStatementTail  { If $2 $4 $5 }
    | for VarName in Expr ':' Suite    { For $2 $4 $6 }

-- statements
Suite :: { [Sentence] }
    : list1(newline) indent list(newline) list1(Statement) dedent { $4 }
Statement :: { Sentence }
    : SimpleStatement list1(newline)   { $1 }
    | CompoundStatement list(newline) { $1 }

-- toplevel declarations
ToplevelDecl :: { ToplevelDecl }
    : from sep1(Name, '.') import '*'  { FromImport $2 }
    | def FunName '(' FormalArgs ')' '->' Type ':' Suite { FunDef $2 $4 $7 $9 }
    | VarName ':' Type '=' Expr        { ConstDef $1 $3 $5 }

{
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
