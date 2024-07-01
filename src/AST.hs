module AST (
    Program(..)
  , Block(..)
  , Declaration(..)
  , Var(..)
  , TypeVar(..)
  , CompoundStatement(..)
  , Statement(..)
  , ProcedureStatement(..)
  , ProcedureParams(..)
  , ProcedureBodyStatement(..)
  , ProcedureStatementList(..)
  , Expression(..)
  , Term(..)
  , Factor(..)
  , Identifier(..)
) where

-- | Represents an entire program.
data Program = Program Identifier Block
    deriving (Show, Eq)

-- | Represents a block of declarations, procedure statements, and compound statements.
data Block = Block Declaration (Maybe ProcedureStatement) CompoundStatement
    deriving (Show, Eq)

-- | Represents a declaration consisting of a list of variables.
data Declaration = Declaration [Var]
                 | Empty
    deriving (Show, Eq)

-- | Represents a variable with a list of identifiers and a type.
data Var = Var [Identifier] TypeVar
    deriving (Show, Eq)

-- | Represents the type of a variable, which can be INTEGER or REAL.
data TypeVar = INTEGER
             | REAL
    deriving (Show, Eq)

-- | Represents a compound statement consisting of a list of statements.
data CompoundStatement = CompoundStatement [Statement]
    deriving (Show, Eq)

-- | Represents a statement, which can be a compound statement, assignment, procedure statement, or empty.
data Statement = CompoundStmt CompoundStatement
               | AssignStmt Identifier Expression
               | ProcedureStmt ProcedureStatement
               | EmptyStatement
    deriving (Show, Eq)

data ProcedureStatement = ProcedureStatement Identifier ProcedureParams ProcedureStatementList
    deriving (Show, Eq)

data ProcedureParams = ProcedureParams [Var]
                     | NoProcedureParams
    deriving (Show, Eq)

data ProcedureBodyStatement = ProcedureDeclaration Declaration
                             | ProcedureAssignment Expression
                             | ProcedureEmpty
    deriving (Show, Eq)

data ProcedureStatementList = ProcedureStatementList [ProcedureBodyStatement]
                             | EmptyProcedureStatementList
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

-- | Represents a factor, which can be a positive or negative factor, an integer, a parenthesized expression, or a variable.
data Factor = PlusFactor Factor
            | MinusFactor Factor
            | IntegerFactor Int
            | ParenFactor Expression
            | VariableFactor Identifier
    deriving (Show, Eq)

-- | Represents an identifier.
data Identifier = Identifier String
    deriving (Show, Eq)