:- module fib.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int.
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
  io.nl(!IO).

