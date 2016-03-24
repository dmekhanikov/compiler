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
    | ('+' | '-') expression                    #signedExpr
    | expression ('*' | '/' | '%') expression   #mulDiv
    | expression ('-' | '+') expression         #sum
    | expression ('==' | '!=' | '<=' | '>='
                       | '<' | '>') expression  #comparison
    | expression ('&&' | '||') expression       #junction
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
    : ID ID '(' parameterList? ')' block
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

Z       : [+-]?[1-9][0-9]* ;
B       : ('true' | 'false') ;
ID      : [_a-zA-Z][-_a-zA-Z0-9]* ;
WS      : [ \t\r\n]+ -> skip ;   // skip spaces, tabs, newlines
