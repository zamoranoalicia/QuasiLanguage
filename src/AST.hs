module AST (
    Program(..)
  , Block(..)
  , Declaration(..)
  , Procedure(..)
  , Var(..)
  , TypeVar(..)
  , CompoundStatement(..)
  , Statement(Assign,EmptyStatement)
  , Expression(..)
  , Term(TermFactor)
  , Factor(Value)
  , Identifier(..)
) where

{--
Question 1: EBNF Definition
Update the EBNF definition to include the new procedure declaration syntax.
PROGRAM test;
VAR
xyz,abc:INTEGER;
cdf,wvy :INTEGER;
PROCEDURE Alpha(a : INTEGER);
VAR y:INTEGER;
BEGIN
y := 1 + 3;
END;
BEGIN
END.
1. What changes are necessary in the EBNF to support the declaration of
multiple variables of the same type? 
2. Provide the updated EBNF definition.

Procedure esta a la altura del var y begin, por lo que se agrega a la lista de declaraciones

program ::= PROGRAM ID SEMI block DOT

block ::= declarations compound_statement
          | procedure ID SEMI declarations compound_statement  -- se agrega esta regla, PEOCEDURE Alpha(a:Integer) ; VAR y:Integer; BEGIN y := 1 + 3; END;

declarations ::= VAR (variable_declaration SEMI)+
                 | empty

variable_declaration ::= ID (COMMA ID)* COLON type_spec

type_spec ::= INTEGER | REAL

compound_statement ::= BEGIN statement_list END

statement_list ::= statement
               | statement SEMI statement_list

statement ::= compound_statement
          | assignment_statement
          | empty

assignment_statement ::= variable ASSIGN expr

empty ::=

expr::= term ((PLUS | MINUS) term)*

term::= factor ((MUL | DIV) factor)*

factor ::= PLUS factor
       | MINUS factor
       | INTEGER
       | LPAREN expr RPAREN
       | variable

variable::= ID

Questions

1. Describe the changes needed in the AST to support the new procedure
declaration syntax.
R. Se agrega un nuevo constructor para Procedure que contiene un identificador y un bloque.
1. Explain how the symbol table should be updated to have procedures in
the current program.
R. Se debe agregar un nuevo tipo de simbolo para los procedimientos, que contenga el nombre del procedimiento y el bloque de declaraciones y sentencias compuestas.
1. Describe the changes necessary in the interpreter to support the new syn-
tax.
R. Se debe agregar un nuevo constructor para Procedure en el AST y se debe agregar un nuevo tipo de simbolo para los procedimientos en la tabla de simbolos.
-}

-- | Represents an entire program.
data Program = Program Identifier Block
    deriving (Show, Eq)

-- | Represents a block of declarations and compound statements.
data Block = Block Declaration [CompoundStatement]
             | BlockWithProcedure Declaration [CompoundStatement] [Procedure]
    deriving (Show, Eq)

-- | Represents a procedure declaration.
data Procedure = Procedure Identifier Block
    deriving (Show, Eq)

-- | Represents a declaration consisting of a list of variables.
data Declaration = Declaration [Var]
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
