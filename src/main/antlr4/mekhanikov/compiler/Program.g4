grammar Program;

program
    : (functionDef | structDef)*
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
    | 'new' ID                                  #newExpr
    | ID '.' ID                                 #fieldAccess
    ;

assignmentExpr
    : <assoc=right> ID '=' expression           #varAssignment
    | <assoc=right> ID '.' ID '=' expression    #fieldAssignment
    ;

expressionList
    : expression (',' expression)*
    ;

statement
    : expression ';'                                #exprStmt
    | 'if' '(' expression ')' block 'else' block    #ifStmt
    | 'while' '(' expression ')' block              #whileStmt
    ;

structDef
    : 'struct' ID '{'
        fieldDecl*
      '}'
    ;

fieldDecl
    : PRIVATE? varDecl
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
PRIVATE: 'private';
ID      : [_a-zA-Z][-_a-zA-Z0-9]* ;
SIGN    : [+-] ;
MULDIV  : [*/%] ;
CMP     : ('==' | '!=' | '<=' | '>=' | '<' | '>') ;
JUNCTION: ('&&' | '||') ;
WS      : [ \t\r\n]+ -> skip ;   // skip spaces, tabs, newlines
Comment : '//' ~('\r' | '\n')* -> skip;
