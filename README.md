# QuasiLanguage

1. is it necessary to change block to put procedure and create the definition of procedure with a modified block called procedure_block
3. It is necessary to add these 2 new definitions and add procedure to block to be able to instantiate it
4. It is necessary 4 new parsers, one to parse several procedures, the second for the body of a single procedecure, another with a structure similar to a block only with the change of being able to define another procedure inside and the last one a CompoundStatement with the modification that at the end of the declaration is not "." but ";" and changes in block to be able to instantiate new procedures.
5. It is necessary to add a new table to store the procedures, and a new symbol to store the procedures and finally do all the operations of adding, searching and deleting from the table but with the procedure symbols.