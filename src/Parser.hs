module Parser(parseStatement
            , parseExpression
            , whiteSpace
            , lexer
            , opLetter
            , languageDef
            , dotParse
            , parseCompoundStatement
            , parseAssigment
            , parseStatements
            , parseNewLine) where

import AST as AST
import Text.Parsec.String
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

parseCompoundStatement :: Parser [Statement]
parseCompoundStatement = (string "BEGIN"    *>
                        (whiteSpace lexer)  *>
                        parseStatements     <*
                        string "END"        <*
                        (char '.'))

parseStatements :: Parser [Statement]
parseStatements = ((parseStatement) `sepEndBy` (whiteSpace lexer))

parseStatement :: Parser Statement
parseStatement =  (try parseAssigment) <|>
                  (try parseEmpty)

parseNewLine :: Parser Char
parseNewLine = (char '\n') <|> (char '\r') <|> (char '\t')

parseEmpty :: Parser Statement
parseEmpty = EmptyStatement <$ (parseNewLine)

parseAssigment :: Parser Statement
parseAssigment = Assign <$> (qlIdentifier <* parseAssignSymbol) <*> parseExpression


parseAssignSymbol :: Parser Char
parseAssignSymbol = char ':' <* char '='

languageDef :: LanguageDef st
languageDef = emptyDef
  { commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  , identStart      = letter
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = opLetter emptyDef
  , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedNames   = ["BEGIN", "END"]
  , reservedOpNames = ["+", "-", "*", "/", ":=", "==", "<", ">", "<=", ">="]
  , caseSensitive   = True
  }

lexer :: TokenParser st
lexer = makeTokenParser languageDef

qlIdentifier = P.identifier lexer

dotParse :: Parser Char
dotParse = (char '.')

semicolon :: Parser Char
semicolon = (char ';')

parseExpression :: Parser Expression
parseExpression  = parseOperation <* semicolon

parseOperation :: Parser Expression
parseOperation = parsePlus

parsePlus :: Parser Expression
parsePlus = AST.Plus <$> (parseTermFactor <* parsePlusSign) <*> parseTermFactor

parsePlusSign :: Parser Char
parsePlusSign = char '+'

parseTermFactor :: Parser Term
parseTermFactor = AST.TermFactor <$> parseFactor

parseFactor :: Parser Factor
parseFactor = Value <$> parseDigit

parseDigit :: Parser Int
parseDigit = read <$> many1 digit
