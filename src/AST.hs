module AST (
    Program(..)
  , Block(..)
  , Declaration(..)
  , Var(..)
  , TypeVar(..)
  , CompoundStatement(..)
  , Statement(Assign,EmptyStatement)
  , Expression(..)
  , Term(TermFactor)
  , Factor(Value)
  , Identifier(..)
  ,Procedure(..)
  ,Parameter(..)
) where
import Data.IntMap.Merge.Lazy (SimpleWhenMatched)

-- | Represents an entire program.
data Program = Program Identifier Block
    deriving (Show, Eq)

-- | Represents a block of declarations and compound statements.
data Block = Block Declaration Procedure [CompoundStatement]
    deriving (Show, Eq)

-- | Represents a declaration consisting of a list of variables.
data Declaration = Declaration [Var]
    deriving (Show, Eq)

data Procedure = Procedure VarMult
    deriving (Show, Eq)

data ProcedureDecl = ProcedureDecl Identifier Parameter
    deriving(Show, Eq)

-- | Represents a variable with a list of identifiers and a type.
data Var = Var [Identifier] TypeVar
    deriving (Show, Eq)

data VarMult = VarMult Parameter
    deriving (Show, Eq)

data Parameter = Parameter Identifier TypeVar
    deriving (Show, Eq)

-- | Represents the type of a variable, which can be INTEGER or REAL.
data TypeVar = INTEGER
             | REAL
    deriving (Show, Eq)

-- | Represents a compound statement consisting of a list of statements.
data CompoundStatement = CompoundStatement [Statement]
    deriving (Show, Eq)

-- | Represents a statement, which can be an assignment or an empty statement.
data Statement = Assign Identifier Expression
               | EmptyStatement
    deriving (Show, Eq)

-- | Represents an expression, which can be an addition, subtraction, or a term.
data Expression = Plus Term Term
                | Minus Term Term
                | ETerm Term
    deriving (Show, Eq)

-- | Represents a term, which can be a division, multiplication, or a factor.
data Term = Div Factor Factor
          | Mul Factor Factor
          | TermFactor Factor
    deriving (Show, Eq)

-- | Represents a factor, which is a value.
data Factor = Value Int
    deriving (Show, Eq)

-- | Represents an identifier.
data Identifier = Identifier String
    deriving (Show, Eq)
    
data Procedure = Procedure Identifier [Parameter] Block
    deriving (Show, Eq)

data Parameter = Parameter Identifier TypeVar
    deriving (Show, Eq)