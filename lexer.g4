// DELETE THIS CONTENT IF YOU PUT COMBINED GRAMMAR IN Parser TAB
lexer grammar ExprLexer;

AND : 'and' ;
NOT : 'not' ;
TRUE : 'true' ;
FALSE : 'false' ;
IF : 'if' ;
THEN : 'then' ;
ELSE : 'else' ;
CSKIP : 'skip' ;
WHILE : 'while' ;
DO : 'do' ;
DEFEQ : ':=' ;
COMMA : ',' ;
SEQ : ';' ;
PARA : '||' ;
LPAREN : '(' ;
RPAREN : ')' ;
LT : '<' ;
PLUS : '+' ;
MINUS : '-' ;
MULT : '*' ;
DIV : '/' ;

INT : [0-9]+ ;
ID: [a-zA-Z_][a-zA-Z_0-9]* ;
WS: [ \t\n\r\f]+ -> skip ;
