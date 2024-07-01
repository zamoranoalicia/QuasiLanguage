module Lib
  ( someFunc,
  )
where

import Interpreter
import Parser as P
import SemanticAnalyzer
import Text.Parsec

someFunc :: IO ()
someFunc = do
  content <- readFile "./resources/code.qs"
  let result = parse P.parseProgram "./resources/code.qs" content
  print result
  {- either
    (\err -> putStrLn $ "Parse error: " ++ show err)
    ( \code ->
        let (p, _) = analyzeProgram code
            (_, r) = interpretProgram p
         in print r
    )
    result -}
