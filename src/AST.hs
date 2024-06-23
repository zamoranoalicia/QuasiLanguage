module AST(
            Statement(..)
          , Identifier (..)
          , Expression (..)
          , Term (..)
          , Factor (..)
          , Var(..)
          , TypeVar (INTEGER, REAL)
          , Declaration (QSDeclaration)
          , Block (QSBlock)
          , CompoundStatement (CompStatement)
          , Program (QSProgram)
          ) where

data Program = QSProgram Identifier Block deriving (Show, Eq)

data Block = QSBlock Declaration CompoundStatement deriving (Show, Eq)

data Declaration = QSDeclaration [Var] deriving (Show, Eq)

data Var = Var [Identifier] TypeVar deriving (Show, Eq)

data TypeVar = INTEGER |
               REAL deriving (Show, Eq)

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

data Identifier = Identifier String deriving (Show, Eq)


