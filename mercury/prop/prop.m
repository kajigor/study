:- module prop.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module char, list, string.

:- type bool ---> true; false.

:- type prop ---> conj(prop, prop)
                ; disj(prop, prop)
                ; lit(bool). 

:- pred propToString(prop::in, string::out) is det.
:- pred boolToString(bool::in, string::out) is det.
:- pred and(bool::in, bool::in, bool::out) is det. 
:- pred or(bool::in, bool::in, bool::out) is det. 
%:- pred evaluate(prop::in, bool::out) is det. 
%:- pred evaluate(prop::out, bool::in) is multi. 
:- pred evaluate(prop, bool).
:- mode evaluate(out, in) is nondet. 

boolToString(true, "true"). 
boolToString(false, "false").

propToString(lit(B), R) :- boolToString(B, R). 
propToString(conj(X, Y), R) :- 
  propToString(X, Sx),
  propToString(Y, Sy), 
  R = "conj(" ++ Sx ++ ", " ++ Sy ++ ")". 
propToString(disj(X, Y), R) :- 
  propToString(X, Sx),
  propToString(Y, Sy), 
  R = "disj(" ++ Sx ++ ", " ++ Sy ++ ")". 

and(true, true, true). 
and(false, true, false). 
and(true, false, false). 
and(false, false, false). 

or(false, false, false). 
or(false, true, true). 
or(true, false, true). 
or(true, true, true). 

evaluate(lit(true), true). 
evaluate(lit(false), false). 
evaluate(conj(X, Y), R) :- 
  evaluate(X, Rx), 
  evaluate(Y, Ry), 
  and(Rx, Ry, R). 
evaluate(disj(X, Y), R) :- 
  evaluate(X, Rx), 
  evaluate(Y, Ry), 
  or(Rx, Ry, R). 


main(!IO) :-
 % Prop = conj(lit(true), disj(lit(false), lit(false))), 
 % propToString(Prop, Str), 

 % evaluate(Prop, Res), 
 % boolToString(Res, StrRes),
  
 % io.format("Prop:\t%s\n", [s(Str)], !IO),
 % io.format("Result:\t%s\n", [s(StrRes)], !IO), 

  evaluate(Prop1, true),
  propToString(Prop1, StrProp),
  io.format("EvaluateToTrue:\n%s\n", [s(StrProp)], !IO).
   
