module AST
  ( Program (..),
    Block (..),
    Declaration (..),
    Var (..),
    TypeVar (..),
    CompoundStatement (..),
    Statement (Assign, EmptyStatement),
    Expression (..),
    Term (TermFactor),
    Factor (Value),
    Identifier (..),
    Procedure (..),
    ProcedureParam (..)
  )
where

-- | Represents an entire program.
data Program = Program Identifier Block
  deriving (Show, Eq)

-- | Represents a block of declarations and compound statements.
data Block = Block Declaration [Procedure] [CompoundStatement]
  deriving (Show, Eq)

-- | Represents a declaration consisting of a list of variables.
newtype Declaration = Declaration [Var]
  deriving (Show, Eq)

-- | Represents a variable with a list of identifiers and a type.
data Var = Var [Identifier] TypeVar
  deriving (Show, Eq)

-- | Represents the type of a variable, which can be INTEGER or REAL.
data TypeVar
  = INTEGER
  | REAL
  deriving (Show, Eq)

-- functions
type ProcedureVar = Declaration

type ProcedureStatatement = CompoundStatement

data Procedure = Procedure Identifier [ProcedureParam] ProcedureVar ProcedureStatatement
  deriving (Show, Eq)

data ProcedureParam = ProcedureParam Identifier TypeVar
  deriving (Show, Eq)

-- | Represents a compound statement consisting of a list of statements.
newtype CompoundStatement = CompoundStatement [Statement]
  deriving (Show, Eq)

-- | Represents a statement, which can be an assignment or an empty statement.
data Statement
  = Assign Identifier Expression
  | EmptyStatement
  deriving (Show, Eq)

-- | Represents an expression, which can be an addition, subtraction, or a term.
data Expression
  = Plus Term Term
  | Minus Term Term
  | ETerm Term
  deriving (Show, Eq)

-- | Represents a term, which can be a division, multiplication, or a factor.
data Term
  = Div Factor Factor
  | Mul Factor Factor
  | TermFactor Factor
  deriving (Show, Eq)

-- | Represents a factor, which is a value.
newtype Factor = Value Int
  deriving (Show, Eq)

-- | Represents an identifier.
newtype Identifier = Identifier String
  deriving (Show, Eq)
