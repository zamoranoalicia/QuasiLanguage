## Question 1: EBNF Definition

### 1. What changes are necessary in the EBNF to support the declaration of multiple variables of the same type?

The current EBNF already supports multiple variables of the same type, but to support Procedure declarations, it is necessary to add the procedure definition with the reserved word "Procedure" the identifier, the params between parens followed by semicolon, then the variable declarations, then a compound statement and a semicolon, also we need to add this procedure to the program to have zero or many.

## Question 3: Abstract Syntax Tree (AST)

### 1. Describe the changes needed in the AST to support the new procedure declaration syntax.

The changes needed in the AST are to add the Procedure data with its identifier, params, declarations and compound statement to build a procedure declaration and also adding it to the Block data in an array to be able to build many procedure declarations in the program before the compound statements.


## Question 4: Parser Functions
### 1. List the functions that need to be updated or added to the parser.

The functions that need to be added:
- lparen: to parse '('
- rparen: to parse ')'
- pcolon: to parse a colon ':'
- parseProcedures: to parse zero or many procedures (in an array)
- parseProcedure: to parse a single procedure
- parseParams: to parse zero or many params (in an array)
- parseParam: to parse a single param

The functions that need to be updated:
- parseBlock: We need to update this function adding the parse procedures in it before the compund statements as the AST was updated for the Block data.
 
## Question 5: Symbol Table
###
