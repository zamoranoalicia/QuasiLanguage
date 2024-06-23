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
            , parseNewLine
            , parseAssignSymbol
            , qlIdentifier
            , parseType
            , parseIdentifiers
            , parseIdentifier
            , parseVar
            , parseVariables
            , parseDeclaration
            , parseBlock
            , parseProgram) where

import AST as AST
import Text.Parsec.String
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

parseCompoundStatement :: Parser CompoundStatement
parseCompoundStatement = CompStatement <$> (string "BEGIN"     *>
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
parseNewLine = (char '\n') <|>
               (char '\r') <|>
               (char '\t')


parseEmpty :: Parser Statement
parseEmpty = EmptyStatement <$ (parseNewLine)

parseAssigment :: Parser Statement
parseAssigment = Assign <$> (qlIdentifier        <*
                            parseAssignSymbol    <*
                            (whiteSpace lexer))  <*>
                            parseExpression


parseAssignSymbol :: Parser Char
parseAssignSymbol = char ':' <*
                    char '='

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
  , reservedNames   = ["BEGIN", "END", "VAR", "BLOCK"]
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
parsePlus = AST.Plus <$> (parseTermFactor    <*
                         parsePlusSign       <*
                         (whiteSpace lexer)) <*>
                         parseTermFactor

parsePlusSign :: Parser Char
parsePlusSign = char '+'

parseTermFactor :: Parser Term
parseTermFactor = AST.TermFactor <$> parseFactor

parseFactor :: Parser Factor
parseFactor = Value <$> parseDigit

parseDigit :: Parser Int
parseDigit = read <$> many1 digit

parseType :: Parser AST.TypeVar
parseType = ((AST.INTEGER <$ string "INTEGER") <|>
             (AST.REAL <$ string "REAL"))

parseIdentifier :: Parser Identifier
parseIdentifier = (Identifier <$> qlIdentifier)

parseIdentifiers :: Parser [Identifier]
parseIdentifiers = ((many space) *> 
                  parseIdentifier <*
                  (many space)) `sepBy` (char ',')

parseVar :: Parser Var
parseVar = ((\a b -> (AST.Var a b)) <$>
           (parseIdentifiers)       <*>
           ((string ":")            *>
           (parseType <* semicolon)))

parseVariables :: Parser [Var]
parseVariables = ((parseVar) `sepEndBy` (whiteSpace lexer))

parseDeclaration :: Parser Declaration
parseDeclaration = AST.QSDeclaration <$>
                  (string "VAR"      *>
                  (whiteSpace lexer) *>
                  parseVariables)

parseBlock :: Parser Block
parseBlock = QSBlock <$> parseDeclaration <*> parseCompoundStatement

parseProgram :: Parser Program
parseProgram = AST.QSProgram  <$>
          ((string "PROGRAM"  <*
          (whiteSpace lexer)) *>
          ((parseIdentifier   <*
          semicolon)          <*
          (whiteSpace lexer)))<*>
           parseBlock