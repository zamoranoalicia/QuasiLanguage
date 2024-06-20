module AST(
            Statement(..)
          , Identifier
          , Expression (..)
          , Term (..)
          , Factor (..)
          ) where

data Program = Program  Identifier CompoundStatement deriving (Show, Eq)

data Block = Block Declaration CompoundStatement deriving (Show, Eq)

data Declaration = Declaration Var deriving (Show, Eq)

data Var = Var [Identifier] TypeVar deriving (Show, Eq)

data TypeVar = INTERGER | REAL deriving (Show, Eq)

data CompoundStatement = CompStatement [Statement] deriving (Show, Eq)

data Statement = Assign String Expression
               | EmptyStatement
               deriving (Show, Eq)

data Expression = Plus Term Term
                | Minus Term Term
                | ETerm Term
                deriving (Show, Eq)

data Term = Div Factor Factor
          | Mul Factor Factor
          | TermFactor Factor
          deriving (Show, Eq)

data Factor = Value Int deriving (Show, Eq)

type Identifier = String


