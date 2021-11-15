// A good parentheses grammar  
grammar Paren;
start : s EOF;
s : '(' s ')' s
  |  
  ; 
