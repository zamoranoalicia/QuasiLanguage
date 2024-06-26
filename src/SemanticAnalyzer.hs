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
          symbol = ST.Symbol name (ST.builtInTypeFromType typeVar)
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
      (assign, analyzeIdentifier identifier symbolTable)
    where
      analyzeIdentifier varName table =
        let name = ST.nameFromIdentifier varName
        in case ST.lookupSymbol name table of
            Just _  -> table
            Nothing -> error (undefinedVariable name)
analyzeStatement empty@(AST.EmptyStatement) symbolTable = (empty, symbolTable)

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

        