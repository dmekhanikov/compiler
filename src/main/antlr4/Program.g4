grammar Program;

program
    : functionDef*
    ;

expression
    : B
    | Z
    | ID
    | assignmentExpr
    | ID '(' expressionList? ')'
    | '(' expression ')'
    | ('+' | '-') expression
    | expression ('*' | '/' | '%') expression
    | expression ('-' | '+') expression
    | expression ('==' | '!=' | '<=' | '>=' | '<' | '>') expression
    | expression ('&&' | '||') expression
    ;

assignmentExpr
    : <assoc=right> ID '=' expression
    ;

expressionList
    : expression (',' expression)*
    ;

statement
    : expression ';'
    | 'if' '(' expression ')' block 'else' block
    | 'while' '(' expression ')' block
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
