// Expressions 
grammar Expr;

start : e WS EOF;

e : e '*' e
  | e '+' e 
  | NUM
  ; 

NUM : [1-9][0-9]*
    | '0' 
    ;

WS : [ \t\n\r]*;
