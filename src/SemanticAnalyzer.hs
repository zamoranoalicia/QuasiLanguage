module SemanticAnalyzer(analyzeVarDecl) where

import qualified SymbolTable as ST
import qualified AST as AST

analyzeVarDecl :: AST.Var -> ST.SymbolTable -> ST.SymbolTable
analyzeVarDecl (AST.Var identifiers typeVar) symbTable =
     foldl insertOrError symbTable identifiers
  where
    insertOrError table identifier =
      let name = ST.nameFromIdentifier identifier 
          symbol = ST.Symbol name (ST.builtInTypeFromType typeVar)
      in case ST.lookupSymbol name table of
           Just _  -> error (varAlreadyDefinedError name)
           Nothing -> ST.insertSymbol name symbol table

varAlreadyDefinedError :: String -> String
varAlreadyDefinedError varName = "Variable already defined: " ++ varName
