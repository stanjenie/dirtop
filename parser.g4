parser grammar ExprParser;
options { tokenVocab=ExprLexer; }

program
    : cexpr EOF
    ;

aexpr: ID
    | INT
    | aexpr ('*'|'/') aexpr
    | aexpr ('+'|'-') aexpr
    ;
    
bexpr: 'true'
    | 'false'
    | 'not' bexpr
    | aexpr '<' aexpr
    | bexpr 'and' bexpr
    ;

cexpr: ID ':=' aexpr
    | 'skip'
    | 'if' bexpr 'then' cexpr 'else' cexpr
    | 'while' bexpr 'do' cexpr
    | cexpr ';' cexpr
    | cexpr '||' cexpr
    ;
