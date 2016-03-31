grammar Program;

program
    : functionDef*
    ;

expression
    : B                                         #boolConst
    | Z                                         #intConst
    | ID                                        #variable
    | assignmentExpr                            #assignment
    | ID '(' expressionList? ')'                #functionCall
    | '(' expression ')'                        #parens
    | SIGN expression                           #signedExpr
    | expression MULDIV expression              #mulDiv
    | expression SIGN expression                #sum
    | expression CMP expression                 #comparison
    | expression JUNCTION expression            #junction
    ;

assignmentExpr
    : <assoc=right> ID '=' expression
    ;

expressionList
    : expression (',' expression)*
    ;

statement
    : expression ';'                                #exprStmt
    | 'if' '(' expression ')' block 'else' block    #ifStmt
    | 'while' '(' expression ')' block              #whileStmt
    ;

functionDef
    : ID ID '(' parameterList? ')' '{'
        varDecl*
        statement*
        'return' expression? ';'
      '}'
    ;

varDecl
    : ID ID (',' ID)* ';'
    ;

block
    : '{' statement* '}'
    ;

parameter
    : ID ID
    ;

parameterList
    : parameter (',' parameter)*
    ;

Z       : [+-]?('0'|[1-9][0-9]*) ;
B       : ('true' | 'false') ;
ID      : [_a-zA-Z][-_a-zA-Z0-9]* ;
SIGN    : [+-] ;
MULDIV  : [*/%] ;
CMP     : ('==' | '!=' | '<=' | '>=' | '<' | '>') ;
JUNCTION: ('&&' | '||') ;
WS      : [ \t\r\n]+ -> skip ;   // skip spaces, tabs, newlines
Comment : '//' ~('\r' | '\n')* -> skip;
