grammar Program;

program
    : (functionDef | structDef)*
    ;

expression
    : B                                                 #boolConst
    | Z                                                 #intConst
    | ID                                                #variable
    | ID '(' expressionList? ')'                        #functionCall
    | '(' expression ')'                                #parens
    | SIGN expression                                   #signedExpr
    | expression '.' ID                                 #fieldRead
    | expression '.' ID '(' expressionList? ')'         #methodCall
    | expression MULDIV expression                      #mulDiv
    | expression SIGN expression                        #sum
    | expression CMP expression                         #comparison
    | expression JUNCTION expression                    #junction
    | 'new' ID '(' expressionList? ')'                  #newExpr
    | <assoc=right> expression '.' ID '=' expression    #fieldWrite
    | <assoc=right> ID '=' expression                   #varAssignment
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
    : 'struct' ID parentStructDecl? '{'
        memberDecl*
      '}'
    ;

parentStructDecl
    : ':' ID
    ;

memberDecl
    : PRIVATE? (fieldDecl | functionDef | constructorDef)
    ;

fieldDecl
    : ID ID (',' ID)* ';'
    ;

functionDef
    : ID ID '(' parameterList? ')' functionBody
    ;

constructorDef
    : 'constructor' '(' parameterList? ')' functionBody
    ;

functionBody:
    '{'
      varDecl*
      statement*
      returnStmt?
    '}'
    ;

returnStmt
    : 'return' expression? ';'
    ;

varDecl
    : ID varInit (',' varInit)* ';'
    ;

varInit
    : ID ('=' expression)?
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
