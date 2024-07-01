module SymbolTable(
                    insertSymbol
                  , lookupSymbol
                  , deleteSymbol
                  , SymbolTable
                  , Scope(..)
                  , Symbol(..)
                  , SymbolValue(SymbolValue)
                  , builtInTypeFromType
                  , nameFromIdentifier
                  , symbolValueFromValue
                  , valueFromSymbolValue
                  , BuiltInType(INTEGER)
                  ) where

import qualified Data.Map as Map
import qualified AST as AST


type SymbolTable = Map.Map String Symbol

type ProcedureTable = Map.Map String ProcedureSymbol

data ProcedureSymbol = Procedure String  

insertProcedureSymbol :: String -> ProcedureSymbol -> ProcedureTable -> ProcedureTable
insertProcedureSymbol name procedure procedureTable = Map.insert name procedure procedureTable

lookupProcedureSymbol :: String -> ProcedureTable -> Maybe ProcedureSymbol
lookupProcedureSymbol name table = Map.lookup name table

deleteProcedureSymbol :: String -> ProcedureTable -> ProcedureTable
deleteProcedureSymbol name procedureTable = Map.delete name procedureTable

valueFromProcedureValue :: ProcedureSymbol -> String
valueFromProcedureValue (Procedure v) = v

nameFromProcedure :: AST.Procedure -> String 
nameFromProcedure (name _ _) = nameFromIdentifier name

data Scope = GLOBAL|
             LOCAL |
             BLOCK deriving (Show,Eq)

data BuiltInType = INTEGER | REAL deriving (Show, Eq)

data Symbol = Symbol String BuiltInType SymbolValue deriving (Show, Eq)

data SymbolValue = SymbolValue Int deriving(Show, Eq)

insertSymbol :: String -> Symbol -> SymbolTable -> SymbolTable
insertSymbol name symbol symbolTable = Map.insert name symbol symbolTable

lookupSymbol :: String -> SymbolTable -> Maybe Symbol
lookupSymbol name table = Map.lookup name table

deleteSymbol :: String -> SymbolTable -> SymbolTable
deleteSymbol name symbolTable = Map.delete name symbolTable

builtInTypeFromType:: AST.TypeVar -> BuiltInType
builtInTypeFromType AST.INTEGER = INTEGER
builtInTypeFromType AST.REAL = REAL

nameFromIdentifier :: AST.Identifier -> String
nameFromIdentifier (AST.Identifier s) = s

symbolValueFromValue :: AST.Factor -> SymbolValue
symbolValueFromValue (AST.Value value) = SymbolValue value

valueFromSymbolValue :: SymbolValue -> Int
valueFromSymbolValue (SymbolValue v) = v