module Jikka.Serializer.CPlusPlus.Type where

type Name = String

data Type
  = TyVoid
  | TyInt32
  | TyInt64
  | TyBool
  | TyVector Type
  | TyArray Type Int
  | TyFunction Type [Type]
  deriving (Eq, Ord, Show, Read)

data RepType
  = AscTo Name
  | AscFromTo Name Name
  | DecTo Name Name
  | DecFromTo Name Name
  deriving (Eq, Ord, Show, Read)

data Sentence
  = Decl (Maybe Type) Name (Maybe Expr)
  | Assign Expr Expr
  | If Expr Sentence Sentence
  | Rep Name RepType Sentence
  | Block [Sentence]
  | Return Expr
  deriving (Eq, Ord, Show, Read)

data Literal
  = Int32 Integer
  | Int64 Integer
  | Bool Bool
  deriving (Eq, Ord, Show, Read)

data BuiltIn
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Negate Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr Expr
  | At Expr Expr
  | Vector Type Expr (Maybe Expr)
  deriving (Eq, Ord, Show, Read)

data Expr
  = Var Name
  | Lit Literal
  | BuiltIn BuiltIn
  | App Expr [Expr]
  deriving (Eq, Ord, Show, Read)

data ToplevleDecl
  = FunDecl Name [(Name, Type)] [Sentence]
  | ConstDecl Name Literal
  deriving (Eq, Ord, Show, Read)

data IncludeType
  = IncludeSystem
  | IncludeQuote
  deriving (Eq, Ord, Show, Read)

data Program
  = Program
      { includes :: [(IncludeType, Name)],
        decls :: [ToplevleDecl]
      }
  deriving (Eq, Ord, Show, Read)
