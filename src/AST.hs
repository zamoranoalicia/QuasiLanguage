module AST (
    Program(..)
  , Block(..)
  , Declaration(..)
  , Var(..)
  , TypeVar(..)
  , CompoundStatement(..)
  , Statement(..)
  , ProcedureDeclaration(..)
  , Parameter(..)
  , Expression(..)
  , Term(..)
  , Factor(..)
  , Identifier(..)
) where

data Program = Program Identifier Block
    deriving (Show, Eq)

data Block = Block [Declaration] [ProcedureDeclaration] CompoundStatement
    deriving (Show, Eq)

data Declaration = Declaration [Var]
    deriving (Show, Eq)

data Var = Var [Identifier] TypeVar
    deriving (Show, Eq)

data TypeVar = INTEGER
             | REAL
    deriving (Show, Eq)

data CompoundStatement = CompoundStatement [Statement]
    deriving (Show, Eq)

data Statement = Assign Identifier Expression
               | EmptyStatement
    deriving (Show, Eq)

data ProcedureDeclaration = ProcedureDeclaration Identifier [Parameter] ([Declaration], [Statement])
    deriving (Show, Eq)

data Parameter = Parameter Identifier TypeVar
    deriving (Show, Eq)

data Expression = Plus Term Term
                | Minus Term Term
                | ETerm Term
    deriving (Show, Eq)

data Term = Div Factor Factor
          | Mul Factor Factor
          | TermFactor Factor
    deriving (Show, Eq)

data Factor = Value Int
    deriving (Show, Eq)

data Identifier = Identifier String
    deriving (Show, Eq)
