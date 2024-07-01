module Parser (
    parseStatement
  , parseExpression
  , qsWhiteSpace
  , lexer
  , opLetter
  , languageDef
  , dotParse
  , parseCompoundStatement
  , parseAssignment
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
  , parseProgram
) where

import AST
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

-- Language definition for the lexer
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
    , reservedNames   = ["BEGIN"
                         , "END"
                         , "VAR"
                         , "BLOCK"
                         , "PROGRAM"
                         , "INTEGER"
                         , "REAL"]
    , reservedOpNames = ["+", "-", "*", "/", ":="
                         , "==", "<", ">", "<=", ">="]
    , caseSensitive   = True
    }

lexer :: TokenParser st
lexer = makeTokenParser languageDef

qsWhiteSpace :: Parser ()
qsWhiteSpace = P.whiteSpace lexer

qlIdentifier :: Parser String
qlIdentifier = P.identifier lexer

dotParse :: Parser Char
dotParse = char '.'

semicolon :: Parser Char
semicolon = char ';'

-- Parsing compound statements
parseCompoundStatement :: Parser CompoundStatement
parseCompoundStatement = CompoundStatement <$>
    (string "BEGIN"  *>
     qsWhiteSpace    *>
     parseStatements <*
     string "END"    <*
     dotParse)

parseStatements :: Parser [Statement]
parseStatements = parseStatement `sepEndBy` qsWhiteSpace

parseStatement :: Parser Statement
parseStatement = try parseAssignment <|> parseEmpty

-- Parsing empty statements (new lines)
parseNewLine :: Parser Char
parseNewLine = char '\n' <|> char '\r' <|> char '\t'

parseEmpty :: Parser Statement
parseEmpty = EmptyStatement <$ parseNewLine

-- Parsing assignment statements
parseAssignment :: Parser Statement
parseAssignment = Assign <$> (parseIdentifier   <*
                              parseAssignSymbol <*
                              qsWhiteSpace)<*>parseExpression

parseAssignSymbol :: Parser String
parseAssignSymbol = string ":="

-- Parsing expressions
parseExpression :: Parser Expression
parseExpression = parseOperation <* semicolon

parseOperation :: Parser Expression
parseOperation = parsePlus

parsePlus :: Parser Expression
parsePlus = Plus <$> (parseTermFactor <*
                      parsePlusSign   <*
                      qsWhiteSpace)   <*> parseTermFactor

parsePlusSign :: Parser Char
parsePlusSign = char '+'

-- Parsing terms and factors
parseTermFactor :: Parser Term
parseTermFactor = TermFactor <$> parseFactor

parseFactor :: Parser Factor
parseFactor = Value <$> parseDigit

parseDigit :: Parser Int
parseDigit = read <$> many1 digit

-- Parsing types
parseType :: Parser TypeVar
parseType = (INTEGER <$ string "INTEGER") <|>
            (REAL <$ string "REAL")

-- Parsing identifiers
parseIdentifier :: Parser Identifier
parseIdentifier = Identifier <$> qlIdentifier

parseIdentifiers :: Parser [Identifier]
parseIdentifiers = parseIdentifier `sepBy` 
                    (char ',' *> qsWhiteSpace)

-- Parsing variables
parseVar :: Parser Var
parseVar = Var <$> parseIdentifiers <*> (string ":" *>
                                         parseType  <*
                                         semicolon)

parseVariables :: Parser [Var]
parseVariables = parseVar `sepEndBy` qsWhiteSpace

-- Parsing declarations
parseDeclaration :: Parser Declaration
parseDeclaration = Declaration <$> (string "VAR" *>
                                    qsWhiteSpace *>
                                    parseVariables)

-- Parsing blocks
parseBlock :: Parser Block
parseBlock = Block <$> parseDeclaration <*> parseCompoundStatements
            <|> BlockProcedure <$> parseProcedure <*> parseCompoundStatementsProcedure

parseCompoundStatements :: Parser [CompoundStatement]
parseCompoundStatements = parseCompoundStatement `sepBy`
                                 qsWhiteSpace

-- Parsing entire programs
parseProgram :: Parser Program
parseProgram = Program <$>
    (string "PROGRAM" *> 
     qsWhiteSpace     *>
     parseIdentifier  <* semicolon <* qsWhiteSpace) <*>
    parseBlock

parseCompoundStatementsProcedure :: Parser CompoundStatementProcedure
parseCompoundStatementsProcedure = CompoundStatementProcedure <$> (string "BEGIN"  *>
     qsWhiteSpace    *>
     parseStatements <*
     string "END"    <*
     semicolon)

parseProcedure :: Parser Procedure
parseProcedure = Procedure <$> (string "PROCEDURE" *> qsWhiteSpace *> identifierTypeVar)

identifierTypeVar :: Parser IdentifierTypeVar
identifierTypeVar = IdentifierTypeVar 