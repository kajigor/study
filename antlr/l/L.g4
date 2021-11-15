// Expressions 
grammar L;

start : stmt* WS? EOF;

stmt : ID ':=' e ';'
     | 'read' '(' ID ')' ';'
     | 'write' '(' e ')' ';'
     | 'if' '(' e ')' '{' stmt* '}' 'else' '{' stmt* '}'
     | 'while' '(' e ')' '{' stmt* '}'
     ;  

e : '-' e 
  | <assoc=right> e pow e 
  | e mul e
  | e add e
  | e compare e 
  | baseE 
  ; 

pow : '^'; 

mul : '*' | '/'; 

add : '+' | '-'; 

compare : '==' | '/=' | '<' | '<=' | '>' | '>=';

baseE : NUM 
      | ID 
      | '(' e ')'
      ;

NUM : [1-9][0-9]*
    | '0' 
    ;

ID : [a-zA-Z][a-zA-Z0-9]*;

WS : [ \t\n\r]* -> skip;
