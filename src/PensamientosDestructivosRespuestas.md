Question 1: EBNF Definition
Update the EBNF definition to include the new procedure declaration syntax.
PROGRAM test;
VAR
xyz,abc:INTEGER;
cdf,wvy :INTEGER;
PROCEDURE Alpha(a : INTEGER);
VAR y:INTEGER;
BEGIN
y := 1 + 3;
END;
BEGIN
END.
1. What changes are necessary in the EBNF to support the declaration of
multiple variables of the same type? 
2. Provide the updated EBNF definition.

Procedure esta a la altura del var y begin, por lo que se agrega a la lista de declaraciones

program ::= PROGRAM ID SEMI block DOT

block ::= declarations compound_statement
          | procedure ID SEMI declarations compound_statement  -- se agrega esta regla, PEOCEDURE Alpha(a:Integer) ; VAR y:Integer; BEGIN y := 1 + 3; END;

declarations ::= VAR (variable_declaration SEMI)+
                 | empty

variable_declaration ::= ID (COMMA ID)* COLON type_spec

type_spec ::= INTEGER | REAL

compound_statement ::= BEGIN statement_list END

statement_list ::= statement
               | statement SEMI statement_list

statement ::= compound_statement
          | assignment_statement
          | empty

assignment_statement ::= variable ASSIGN expr

empty ::=

expr::= term ((PLUS | MINUS) term)*

term::= factor ((MUL | DIV) factor)*

factor ::= PLUS factor
       | MINUS factor
       | INTEGER
       | LPAREN expr RPAREN
       | variable

variable::= ID

Questions

1. Describe the changes needed in the AST to support the new procedure
declaration syntax.
R. Se agrega un nuevo constructor para Procedure que contiene un identificador y un bloque.
1. Explain how the symbol table should be updated to have procedures in
the current program.
R. Se debe agregar un nuevo tipo de simbolo para los procedimientos, que contenga el nombre del procedimiento y el bloque de declaraciones y sentencias...
1. Describe the changes necessary in the interpreter to support the new syn-
tax.
R. Se debe agregar un nuevo constructor para Procedure en el AST y se debe agregar un nuevo tipo de simbolo para los procedimientos en la tabla de simbolos... 
