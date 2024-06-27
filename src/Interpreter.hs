module Interpreter where

import qualified AST as AST

interpretFactor :: AST.Factor -> Int
interpretFactor (AST.Value v) = v

interpretTermFactor :: AST.Term -> Int
interpretTermFactor (AST.TermFactor v) = interpretFactor v

interpretExpression :: AST.Expression -> Int
interpretExpression (AST.Plus x y ) = (interpretTermFactor x) + (interpretTermFactor y)
interpretExpression (AST.Minus x y ) = (interpretTermFactor x) - (interpretTermFactor y)
interpretExpression (AST.ETerm t) = interpretTermFactor t

interpretStatement :: AST.Statement -> Int
interpretStatement (AST.Assign _ exp) = interpretExpression exp
interpretStatement (AST.EmptyStatement) = 0

