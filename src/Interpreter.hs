module Interpreter where

import qualified AST as AST
import qualified SymbolTable as ST
import qualified SemanticAnalyzer as SA
import qualified Data.Map.Strict as M


data InterpreterOutput = Environment
                        { 
                            values :: M.Map String Int
                        }
                        deriving (Show, Eq)

interpretFactor :: AST.Factor -> Int
interpretFactor (AST.Value v) = v

interpretTermFactor :: AST.Term -> Int
interpretTermFactor (AST.TermFactor v) = interpretFactor v

interpretExpression :: AST.Expression -> Int
interpretExpression (AST.Plus x y ) = (interpretTermFactor x) + (interpretTermFactor y)
interpretExpression (AST.Minus x y ) = (interpretTermFactor x) - (interpretTermFactor y)
interpretExpression (AST.ETerm t) = interpretTermFactor t

interpretStatement :: AST.Statement -> ST.SymbolTable -> (String, Int)
interpretStatement (AST.Assign id exp) table = (ST.nameFromIdentifier id, getExpValue id exp table)
    where
        getExpValue id expression symbolTable =
            let name = ST.nameFromIdentifier id
                symbol = (ST.lookupSymbol name symbolTable)
            in case symbol of
                Just (ST.Symbol _ _ value) -> (ST.valueFromSymbolValue value)
                Nothing -> (interpretExpression expression)
interpretStatement (AST.EmptyStatement) _ = ("",0)

interpreCompStatement :: AST.CompoundStatement -> ST.SymbolTable -> [(String, Int)]
interpreCompStatement (AST.CompoundStatement stmts) table = map (`interpretStatement` table) stmts
{-
interpretBlock :: AST.Block -> ST.SymbolTable -> [[(String, Int)]]
interpretBlock block@(AST.Block declaration compStatements) table = updatedTable block table
            where
                updatedTable block table =
                    let (blockNode, newTable) = (SA.analyzeBlock block table)
                    in map (`interpreCompStatement` newTable) compStatements

-}
