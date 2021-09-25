:- module fib.
:- interface.
:- pred main(io::di, io::uo) is det.
:- import_module io.

:- implementation.


:- import_module list.
:- import_module string.
:- import_module int.

:- pred loop(io::di, io::uo) is det.
:- pred fib(int::in, int::out) is det.
:- func fibFunc(int) = int.
:- func fibExp(int) = int.

fib(N, X) :-
  ( if N =< 2
    then X = 1
    else fib(N - 1, A), fib(N - 2, B), X = A + B
  ).

fibFunc(N) = X :-
  ( if N =< 2
    then X = 1
    else X = fibFunc(N - 1) + fibFunc(N - 2)
  ).

fibExp(N) = ( if N =< 2 then 1 else fibExp(N - 1) + fibExp(N - 2) ).

loop(!IO) :- 
  io.read_line_as_string(Result, !IO), 
  (
    Result = ok(String), 
    ( if string.to_int(string.strip(String), N) 
      then io.format("fib(%d) = %d\n", [i(N), i(fibExp(N))], !IO) 
      else io.format("That isn't a number...\n", [], !IO)
    ), 
    loop(!IO)
  ; 
    Result = eof, 
    io.format("Bye!\n", [], !IO)
  ; 
    Result = error(ErrorCode), 
    io.format("%s\n", [s(io.error_message(ErrorCode))], !IO)
  ).

%loop(!IO) :- 
%  io.read_line_as_string(Result, !IO), 
%  ( if
%      Result = ok(String), 
%      string.to_int(string.strip(String), N)
%    then 
%      io.format("fib(%d) = %d\n", [i(N), i(fibExp(N))], !IO),
%      loop(!IO)
%    else 
%      io.format("That isn't a number...\n", [], !IO)
%  ).
  

main(!IO) :-
  fib(17, X),
  io.write_string("fib(17, ", !IO),
  io.write_int(X, !IO),
  io.write_string(")\n", !IO),
  io.write_string("fibFunc(17) = ", !IO),
  io.write_int(fibFunc(17), !IO),
  io.nl(!IO),
  io.write_string("fibExp(17) = ", !IO),
  io.write_int(fibExp(17), !IO),
  io.nl(!IO),

  loop(!IO).
