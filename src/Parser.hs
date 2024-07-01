module Parser
  ( parseStatement,
    parseExpression,
    qsWhiteSpace,
    lexer,
    opLetter,
    languageDef,
    dotParse,
    parseCompoundStatement,
    parseAssignment,
    parseStatements,
    parseNewLine,
    parseAssignSymbol,
    qlIdentifier,
    parseType,
    parseIdentifiers,
    parseIdentifier,
    parseVar,
    parseVariables,
    parseDeclaration,
    parseBlock,
    parseProgram,
    parseProcedure,
    parseProcedures,
  )
where

import AST
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String
import Text.Parsec.Token
import qualified Text.Parsec.Token as P

-- Language definition for the lexer
languageDef :: LanguageDef st
languageDef =
  emptyDef
    { commentStart = "/*",
      commentEnd = "*/",
      commentLine = "//",
      nestedComments = True,
      identStart = letter,
      identLetter = alphaNum <|> oneOf "_'",
      opStart = opLetter emptyDef,
      opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      reservedNames =
        [ "BEGIN",
          "END",
          "VAR",
          "BLOCK",
          "PROGRAM",
          "INTEGER",
          "REAL"
        ],
      reservedOpNames =
        [ "+",
          "-",
          "*",
          "/",
          ":=",
          "==",
          "<",
          ">",
          "<=",
          ">="
        ],
      caseSensitive = True
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
parseCompoundStatement =
  CompoundStatement
    <$> ( string "BEGIN"
            *> qsWhiteSpace
            *> parseStatements
            <* string "END"
            <* dotParse
        )

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
parseAssignment =
  Assign
    <$ qsWhiteSpace
    <*> ( parseIdentifier
            <* qsWhiteSpace
            <* parseAssignSymbol
            <* qsWhiteSpace
        )
    <* qsWhiteSpace
    <*> parseExpression

parseAssignSymbol :: Parser String
parseAssignSymbol = string ":="

-- Parsing expressions
parseExpression :: Parser Expression
parseExpression = parseOperation <* semicolon

parseOperation :: Parser Expression
parseOperation = parsePlus

parsePlus :: Parser Expression
parsePlus =
  Plus
    <$> ( parseTermFactor
            <* qsWhiteSpace
            <* parsePlusSign
            <* qsWhiteSpace
            <* qsWhiteSpace
        )
    <*> parseTermFactor

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
parseType =
  (INTEGER <$ string "INTEGER")
    <|> (REAL <$ string "REAL")

-- Parsing identifiers
parseIdentifier :: Parser Identifier
parseIdentifier
    = Identifier
    <$ qsWhiteSpace
    <*> qlIdentifier
    <* qsWhiteSpace

parseIdentifiers :: Parser [Identifier]
parseIdentifiers =
  parseIdentifier
    `sepBy` (char ',' *> qsWhiteSpace)

-- Parsing variables
parseVar :: Parser Var
parseVar =
  Var
    <$ qsWhiteSpace
    <*> parseIdentifiers
    <* qsWhiteSpace
    <* string ":"
    <* qsWhiteSpace
    <*> parseType
    <* qsWhiteSpace
    <* semicolon
    <* qsWhiteSpace

parseVariables :: Parser [Var]
parseVariables = parseVar `sepEndBy` newline

-- Parsing declarations
parseDeclaration :: Parser Declaration
parseDeclaration =
  Declaration
    <$ qsWhiteSpace
    <* string "VAR"
    <* qsWhiteSpace
    <*> parseVariables
    <* qsWhiteSpace
    <* qsWhiteSpace

-- Parsing blocks
parseBlock :: Parser Block
parseBlock =
  Block
    <$ qsWhiteSpace
    <*> parseDeclaration
    <* qsWhiteSpace
    <*> parseProcedures
    <* qsWhiteSpace
    <*> parseCompoundStatements
    <* qsWhiteSpace

parseProcedures :: Parser [Procedure]
parseProcedures = many parseProcedure

parseCompoundStatements :: Parser [CompoundStatement]
parseCompoundStatements =
  parseCompoundStatement
    `sepBy` qsWhiteSpace

-- procedure
parseProcedure :: Parser Procedure
parseProcedure =
  Procedure
    <$ qsWhiteSpace
    <* string "PROCEDURE"
    <* qsWhiteSpace
    <*> parseIdentifier
    <* qsWhiteSpace
    <* string "("
    <* qsWhiteSpace
    <*> parseProcedureParams
    <* qsWhiteSpace
    <* string ")"
    <* qsWhiteSpace
    <* string ""
    <* qsWhiteSpace
    <*> parseDeclaration
    <* qsWhiteSpace
    <*> parseProcedureStatement

parseProcedureParams :: Parser [ProcedureParam]
parseProcedureParams = sepBy parseProcedureParam (char ',')

parseProcedureParam :: Parser ProcedureParam
parseProcedureParam =
  ProcedureParam
    <$ qsWhiteSpace
    <*> parseIdentifier
    <* qsWhiteSpace
    <* string ":"
    <* qsWhiteSpace
    <*> parseType

parseProcedureStatement :: Parser CompoundStatement
parseProcedureStatement =
  CompoundStatement
    <$ qsWhiteSpace
    <* string "BEGIN"
    <* qsWhiteSpace
    <*> parseStatements
    <* qsWhiteSpace
    <* string "END"
    <* qsWhiteSpace
    <* string ";"
    <* qsWhiteSpace

-- Parsing entire programs
parseProgram :: Parser Program
parseProgram =
  Program
    <$ qsWhiteSpace
    <* string "PROGRAM"
    <* qsWhiteSpace
    <*> parseIdentifier
    <* qsWhiteSpace
    <* semicolon
    <* qsWhiteSpace
    <*> parseBlock
    <* qsWhiteSpace