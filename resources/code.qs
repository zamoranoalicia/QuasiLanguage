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

/* What changes are necessary in the EBNF to support the declaration of
multiple variables of the same type? */
// R. En el EBNF, en este caso no se especifica la cantidad de veces que puede ser declarada
// entonces, en este caso se puede asumir que es de 1 a muchos por lo que deberia de utilizarse
// ()+ dentro de la declaracion definida de nuestro EBNF. 

