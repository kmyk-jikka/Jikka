{
module Jikka.Deserializer.ML.Parser (run) where

import Data.List (intercalate)
import qualified Jikka.Deserializer.ML.Lexer as L (Token(..))
import Jikka.Deserializer.ML.Pos
import qualified Jikka.Deserializer.ML.ShuntingYard as ShuntingYard
import Jikka.Deserializer.ML.ShuntingYard (Op, getOpName, getBuiltInOp)
import Jikka.Deserializer.ML.Type

-- TODO: use left-recursion for effciency
}

%name run
%tokentype { WithPos L.Token }
%monad { Either String }
%error { happyErrorExpList }
%errorhandlertype explist

%token
    unit            { WithPos _ L.Unit }
    int             { WithPos _ (L.Int _) }
    bool            { WithPos _ (L.Bool _) }

    let             { WithPos _ L.Let }
    rec             { WithPos _ L.Rec }
    in              { WithPos _ L.In }
    fun             { WithPos _ L.Fun }
    function        { WithPos _ L.Function }
    match           { WithPos _ L.Match }
    with            { WithPos _ L.With }
    end             { WithPos _ L.End }
    if              { WithPos _ L.If }
    then            { WithPos _ L.Then }
    else            { WithPos _ L.Else }
    given           { WithPos _ L.Given }

    '->'            { WithPos _ L.Arrow }
    '|'             { WithPos _ L.Bar }
    ':'             { WithPos _ L.Colon }
    ','             { WithPos _ L.Comma }
    '='             { WithPos _ L.Equal }
    ';'             { WithPos _ L.Semicolon }
    '_'             { WithPos _ L.Underscore }

    '{'             { WithPos _ L.OpenBrace }
    '['             { WithPos _ L.OpenBracket }
    '('             { WithPos _ L.OpenParen }
    '}'             { WithPos _ L.CloseBrace }
    ']'             { WithPos _ L.CloseBracket }
    ')'             { WithPos _ L.CloseParen }

    ident           { WithPos _ (L.Ident _) }
    '+'             { WithPos _ (L.Op "+") }
    op              { WithPos _ (L.Op _) }

%%

Start :: { Program }
    : GivenDecls Expr                  { Program (reverse $1) $2 }

Int :: { WithPos Integer }
    : int                              { withPos $1 $ let (L.Int n) = value $1 in n }
Bool :: { WithPos Bool }
    : bool                             { withPos $1 $ let (L.Bool p) = value $1 in p }
Ident :: { WithPos Name }
    : ident                            { withPos $1 $ let (L.Ident s) = value $1 in s }
Op :: { WithPos Op }
    : op                               {% let (L.Op s) = value $1 in getBuiltInOp $ withPos $1 s }
    | '+'                              {% let (L.Op s) = value $1 in getBuiltInOp $ withPos $1 s }

Type :: { WithPos Type }
    : Ident                            { withPos $1 $ TyVar (value $1) }
    | Ident '->' Type                  { withPos $1 $ TyFun (TyVar $ value $1) (value $3) }
    | '(' Type ')'                     { $2 }

GivenDecl :: { (Name, WithPos Type) }
    : let given Ident ':' Type in    { (value $3, $5) }

GivenDecls :: { [(Name, WithPos Type)] }
    : {- empty -}                      { [] }
    | GivenDecls GivenDecl             { $2 : $1 }

OptName :: { Maybe Name }
    : '_'                              { Nothing }
    | Ident                            { Just (value $1) }

OptType :: { Maybe (WithPos Type) }
    : {- empty -}                      { Nothing }
    | ':' Type                         { Just $2 }

Arg1 :: { (Maybe Name, Maybe (WithPos Type)) }
    : OptName                          { ($1, Nothing) }
    | '(' OptName ':' Type ')'         { ($2, Just $4) }

Args :: { [(Maybe Name, Maybe (WithPos Type))] }
    : {- empty -}                      { [] }
    | Arg1 Args                        { $1 : $2 }

Literal :: { WithPos Literal }
    : unit                             { withPos $1 Unit }
    | Int                              { withPos $1 $ Int (value $1) }
    | Bool                             { withPos $1 $ Bool (value $1) }

MatchPattern :: { MatchPattern }
    : OptName                          { PatVar $1 }
    | Literal                          { PatLit $ value $1 }
    | OptName '+' Int                  { PatPlusK $1 (value $3) }
MatchPatterns :: { [MatchPattern] }
    : MatchPattern                     { [$1] }
    | MatchPattern MatchPatterns       { $1 : $2 }
MatchBranch :: { MatchBranch }
    : '|' MatchPatterns '->' Expr      { ($2, $4) }
MatchBranches :: { [MatchBranch] }
    : {- empty -}                      { [] }
    | MatchBranch MatchBranches        { $1 : $2 }

LetType :: { LetType }
    : {- empty -}                      { NoRec }
    | rec                              { Rec }

Expr :: { WithPos Expr }
    : ItemOpSeq                        {% let app op e1 e2 = withPos op (App (withPos op (App (fmap (Var . getOpName) op) e1)) e2) in ShuntingYard.run app $1 }
    | let LetType OptName Args OptType '=' Expr in Expr  { withPos $1 $ Let $2 $3 $4 $5 $7 $9 }
    | match Expr with MatchBranches end  { withPos $1 $ Match $2 $4 }
    | function MatchBranches end       { withPos $1 $ Function $2 }
    | if Expr then Expr else Expr      { withPos $1 $ If $2 $4 $6 }
    | fun Arg1 Args '->' Expr          { withPos $1 $ Fun ($2 : $3) $5 }

ItemAtomic :: { WithPos Expr }
    : Literal                          { withPos $1 $ Lit (value $1) }
    | Ident                            { withPos $1 $ Var (value $1) }
    | '(' Expr ')'                     { $2 }

ItemFunApp :: { WithPos Expr }
    : ItemAtomic                       { $1 }
    | ItemFunApp ItemAtomic            { withPos $1 $ App $1 $2 }

ItemOpSeq :: { (WithPos Expr, [(WithPos Op, WithPos Expr)]) }
    : ItemFunApp                       { ($1, []) }
    | ItemFunApp Op ItemOpSeq          { let (x, ys) = $3 in ($1, ($2, x) : ys) }

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
