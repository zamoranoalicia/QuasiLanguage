module SemanticAnalyzer (
    analyzeVar
  , analyzeDecl
  , analyzeStatement
  , analyzeCompStatement
  , analyzeBlock
) where

import qualified SymbolTable as ST
import qualified AST as AST

-- Analyzes a variable declaration, checking for redefinitions.
-- Inserts the variable into the symbol table if not already defined.
analyzeVar :: AST.Var -> ST.SymbolTable -> (AST.Var, ST.SymbolTable)
analyzeVar var@(AST.Var identifiers typeVar) symbTable =
     (var, foldl insertOrError symbTable identifiers)
  where
    insertOrError table identifier =
      let name = ST.nameFromIdentifier identifier 
          symbol = ST.Symbol name (ST.builtInTypeFromType typeVar) (ST.SymbolValue 0)
      in case ST.lookupSymbol name table of
           Just _  -> error (varAlreadyDefinedError name)
           Nothing -> ST.insertSymbol name symbol table

-- Generates an error message for redefined variables.
varAlreadyDefinedError :: String -> String
varAlreadyDefinedError varName = "Variable already defined: " ++ varName

-- Analyzes a declaration, which is a collection of variable declarations.
-- Updates the symbol table with the new variables.
analyzeDecl :: AST.Declaration -> ST.SymbolTable -> (AST.Declaration, ST.SymbolTable)
analyzeDecl declaration@(AST.Declaration vars) symbolTable =
    (declaration, analyzeVariables symbolTable vars)
    
-- Helper function to analyze a list of variable declarations.
-- Updates the symbol table with each variable.
analyzeVariables :: ST.SymbolTable -> [AST.Var] -> ST.SymbolTable
analyzeVariables = foldl (\table var -> snd (analyzeVar var table))

-- Analyzes a statement, which can be an assignment or an empty statement.
-- Updates the symbol table if the statement is an assignment.
analyzeStatement :: AST.Statement -> ST.SymbolTable -> (AST.Statement, ST.SymbolTable)
analyzeStatement assign@(AST.Assign identifier exp) symbolTable =
      (assign, analyzeStatementExp identifier exp symbolTable)
    where
      analyzeStatementExp varName expression table =
        let name = ST.nameFromIdentifier varName
        in case ST.lookupSymbol name table of
            Just _  -> updateSymbolExpression name expression table
            Nothing -> error (undefinedVariable name)
analyzeStatement empty@(AST.EmptyStatement) symbolTable = (empty, symbolTable)

-- Updates the symbol table with a new value for a given variable.
updateSymbolExpression :: String -> AST.Expression -> ST.SymbolTable -> ST.SymbolTable 
updateSymbolExpression name exp symbolTable = 
          let (value, table, expression) = analyzeExpression exp symbolTable
              symbol = ST.Symbol name ST.INTEGER value
          in ST.insertSymbol name symbol table

-- Analyzes an expression, which can be a term or an operation (plus).
-- Returns the value of the expression and the updated symbol table.
analyzeExpression :: AST.Expression -> ST.SymbolTable -> (ST.SymbolValue, ST.SymbolTable, AST.Expression)
analyzeExpression term@(AST.ETerm x) symbolTable = (fstOfThree (analyzeTerm x symbolTable),
                          sndOfThree (analyzeTerm x symbolTable), term)
analyzeExpression plus@(AST.Plus x y) symbolTable =
  let (xValue, xTable, xTerm) = analyzeTerm x symbolTable
      (yValue, yTable, yTerm) = analyzeTerm y xTable
  in (ST.SymbolValue ((ST.valueFromSymbolValue xValue) + (ST.valueFromSymbolValue yValue)), yTable, plus)

-- Analyzes a term, which can be a factor, multiplication, or division.
-- Returns the value of the term and the updated symbol table.
analyzeTerm :: AST.Term -> ST.SymbolTable -> (ST.SymbolValue, ST.SymbolTable, AST.Term)
analyzeTerm term@(AST.TermFactor factor) symbolTable = (fstOfThree (analyzeFactor factor symbolTable),
                           sndOfThree (analyzeFactor factor symbolTable), term)

-- Helper functions to extract elements from a tuple.
fstOfThree :: (a, b, c) -> a
fstOfThree (x, _, _) = x

sndOfThree :: (a, b, c) -> b
sndOfThree (_, x, _) = x

-- Analyzes a factor, which can be a value or an identifier.
-- Returns the value of the factor and the updated symbol table.
analyzeFactor :: AST.Factor -> ST.SymbolTable -> (ST.SymbolValue, ST.SymbolTable, AST.Factor)
analyzeFactor factor@(AST.Value value) symbolTable = (ST.symbolValueFromValue factor, symbolTable, factor)

-- Generates an error message for undefined variables.
undefinedVariable :: String -> String
undefinedVariable varName = "Variable not in scope: " ++ varName

-- Analyzes a compound statement, which is a collection of statements.
-- Updates the symbol table with each statement.
analyzeCompStatement :: AST.CompoundStatement -> ST.SymbolTable -> (AST.CompoundStatement, ST.SymbolTable)
analyzeCompStatement compound@(AST.CompoundStatement statements) symbolTable =
    (compound, analyzeStatements symbolTable statements)
  
-- Helper function to analyze a list of statements.
-- Updates the symbol table with each statement.
analyzeStatements :: ST.SymbolTable -> [AST.Statement] -> ST.SymbolTable
analyzeStatements = foldl (\table stmt -> snd (analyzeStatement stmt table))

-- Analyzes a block, which consists of declarations and compound statements.
-- Updates the symbol table with the new declarations and statements.
analyzeBlock :: AST.Block -> ST.SymbolTable -> (AST.Block, ST.SymbolTable)
analyzeBlock block@(AST.Block declaration compStatements) symbolTable = 
    (block, analyzeCompStatements updatedSymbolTable compStatements)
  where
    updatedSymbolTable = snd (analyzeDecl declaration symbolTable)

-- Helper function to analyze a list of compound statements.
-- Updates the symbol table with each compound statement.
analyzeCompStatements :: ST.SymbolTable -> [AST.CompoundStatement] -> ST.SymbolTable
analyzeCompStatements = foldl (\table compStmt -> snd (analyzeCompStatement compStmt table))
