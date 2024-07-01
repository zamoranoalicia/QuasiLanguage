module SemanticAnalyzer(analyzeVar
                      , analyzeDecl
                      , analyzeStatement
                      , analyzeCompStatement
                      , analyzeBlock
                      ) where

import qualified SymbolTable as ST
import qualified AST as AST

analyzeVar :: AST.Var -> ST.SymbolTable -> (AST.Var, ST.SymbolTable)
analyzeVar var@(AST.Var identifiers typeVar) symbTable =
     (var, (foldl insertOrError symbTable identifiers))
  where
    insertOrError table identifier =
      let name = ST.nameFromIdentifier identifier 
          symbol = ST.Symbol name (ST.builtInTypeFromType typeVar) (ST.SymbolValue 0)
      in case ST.lookupSymbol name table of
           Just _  -> error (varAlreadyDefinedError name)
           Nothing -> ST.insertSymbol name symbol table

varAlreadyDefinedError :: String -> String
varAlreadyDefinedError varName = "Variable already defined: " ++ varName

analyzeDecl :: AST.Declaration -> ST.SymbolTable -> (AST.Declaration, ST.SymbolTable)
analyzeDecl declaration@(AST.Declaration vars) symbolTable =
    (declaration, analyzeVariables symbolTable vars)
    
analyzeVariables :: ST.SymbolTable -> [AST.Var] -> ST.SymbolTable
analyzeVariables = foldl (\table var -> snd (analyzeVar var table))

analyzeStatement :: AST.Statement -> ST.SymbolTable ->(AST.Statement, ST.SymbolTable)
analyzeStatement assign@(AST.Assign identifier exp) symbolTable =
      (assign, analyzeStatementExp identifier exp symbolTable)
    where
      analyzeStatementExp varName expression table =
        let name = ST.nameFromIdentifier varName
        in case ST.lookupSymbol name table of
            Just _  -> updateSymbolExpression name expression table
            Nothing -> error (undefinedVariable name)
            analyzeStatement stmt@(AST.ProcedureStatement (AST.ProcedureStatement ident params block)) table =
        let name = ST.nameFromIdentifier ident
        symbol = ST.Symbol name ST.PROCEDURE (ST.ProcedureValue params block)
        in case ST.lookupSymbol name table of
            Just _ -> error $ "Procedure already defined: " ++ name
            Nothing -> (stmt, ST.insertSymbol name symbol table)
analyzeStatement empty@(AST.EmptyStatement) symbolTable = (empty, symbolTable)

updateSymbolExpression :: String -> AST.Expression -> ST.SymbolTable -> ST.SymbolTable 
updateSymbolExpression name exp symbolTable = 
          let (value, table, expression) = (analyzeExpression exp symbolTable)
              symbol = ST.Symbol name ST.INTEGER value
          in (ST.insertSymbol name symbol table)
            

analyzeExpression :: AST.Expression -> ST.SymbolTable -> (ST.SymbolValue, ST.SymbolTable,AST.Expression)
analyzeExpression term@(AST.ETerm x) symbolTable = (fstOfThree(analyzeTerm x symbolTable),
                          sndOfThree(analyzeTerm x symbolTable),term)
analyzeExpression plus@(AST.Plus x y) symbolTable =
  let (xValue, xTable, xTerm) = analyzeTerm x symbolTable
      (yValue, yTable, yTerm) = analyzeTerm y xTable
  in ((ST.SymbolValue ( (ST.valueFromSymbolValue xValue) + (ST.valueFromSymbolValue yValue))),yTable,plus)


analyzeTerm :: AST.Term -> ST.SymbolTable -> (ST.SymbolValue, ST.SymbolTable, AST.Term)
analyzeTerm term@(AST.TermFactor factor) symbolTable = (fstOfThree (analyzeFactor factor symbolTable),
                           sndOfThree(analyzeFactor factor symbolTable), term)

fstOfThree :: (a, b, c) -> a
fstOfThree (x, _, _) = x

sndOfThree :: (a, b, c) -> b
sndOfThree (_, x, _) = x

analyzeFactor :: AST.Factor -> ST.SymbolTable -> (ST.SymbolValue, ST.SymbolTable, AST.Factor)
analyzeFactor factor@(AST.Value value) symbolTable = ((ST.symbolValueFromValue factor), symbolTable, factor)

undefinedVariable :: String -> String
undefinedVariable varName = "Variable not in scope: " ++ varName

analyzeCompStatement :: AST.CompoundStatement -> ST.SymbolTable -> (AST.CompoundStatement, ST.SymbolTable)
analyzeCompStatement compound@(AST.CompoundStatement statements) symbolTable =
    (compound, analyzeStatements symbolTable statements)
  
analyzeStatements :: ST.SymbolTable -> [AST.Statement] -> ST.SymbolTable
analyzeStatements = foldl (\table stmt -> snd (analyzeStatement stmt table))

analyzeBlock :: AST.Block -> ST.SymbolTable -> (AST.Block, ST.SymbolTable)
analyzeBlock block@(AST.Block declaration compStatements) symbolTable = 
    (block, analyzeCompStatements updatedSymbolTable compStatements)
  where
    updatedSymbolTable = snd (analyzeDecl declaration symbolTable)

analyzeCompStatements :: ST.SymbolTable -> [AST.CompoundStatement] -> ST.SymbolTable
analyzeCompStatements = foldl (\table compStmt -> snd (analyzeCompStatement compStmt table))

        