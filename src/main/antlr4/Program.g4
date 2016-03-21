grammar Program;

program
    : functionDef*
    ;

expression
    : boolConst
    | intConst
    | variable
    | assignmentExpr
    | functionCall
    | '(' expression ')'
    | ('+' | '-') expression
    | expression ('*' | '/' | '%') expression
    | expression ('-' | '+') expression
    | expression ('==' | '!=' | '<=' | '>=' | '<' | '>') expression
    | expression ('&&' | '||') expression
    ;

assignmentExpr
    : <assoc=right> variable '=' expression
    ;

functionCall
    : function '(' expressionList? ')'
    ;

expressionList
    : expression (',' expression)*
    ;

statement
    : expression ';'
    | ifStmt
    | whileStmt
    ;

ifStmt
    : 'if' '(' expression ')' block 'else' block
    ;

whileStmt
    : 'while' '(' expression ')' block
    ;

functionDef
    : type function '(' parameterList? ')' block
    ;

block
    : '{' statement* '}'
    ;

parameter
    : type variable
    ;

parameterList
    : parameter (',' parameter)*
    ;

boolConst   : B ;
intConst    : Z ;
variable    : ID ;
type        : ID ;
function    : ID ;

Z       : [+-]?[1-9][0-9]* ;
B       : ('true' | 'false') ;
ID      : [_a-zA-Z][-_a-zA-Z0-9]* ;
WS      : [ \t\r\n]+ -> skip ;   // skip spaces, tabs, newlines
