# QuasiLanguage

## Question 1 
What changes are necessary in the EBNF to support the declaration of
multiple variables of the same type?

R. En el EBNF, en este caso no se especifica la cantidad de veces que puede ser declarada entonces, en este caso se puede asumir que es de 1 a muchos por lo que deberia de utilizarse ()+ dentro de la declaracion definida de nuestro EBNF. 

## Question 3:
Describe the changes needed in the AST to support the new procedure
declaration syntax.

R. La unica parate que necesita cambios es nuestro Bloque, ya que estan siendo agregando todos los datos necesarios


## Question 4:
List the functions that need to be updated or added to the parser.


R. La unicas funciones que estan siendo modificadas, son estas, nada mas se adapto:

```
parseBlock :: Parser Block
parseBlock = Block <$> parseDeclaration <*> parseProcedure <*> parseCompoundStatements

parseProcedure :: Parser Procedure
parseProcedure = Procedure <$> (string "PROCEDURE" *> parseIdentifier) <*> (many parserParameters <* char ';') <*> parseBody

parserParameters :: Parser Parameter
parserParameters = Parameter <$> parseIdentifier <*> parseType

parseBody :: Parser Body
parseBody = Body <$> many parseBodyDeclaration <*> (string "BEGIN" *> parseIdentifier) <*> (parseExpression <* semicolon <* string "END" <* semicolon)

parseBodyDeclaration :: Parser BodyDeclaration
parseBodyDeclaration = BodyDeclaration <$> (string "VAR" *> parseIdentifier <* parseAssignSymbol) <*> (parseType <* semicolon)

```
