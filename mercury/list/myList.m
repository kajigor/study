:- module myList.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int, string.

:- func myMap(func(T1) = T2, list(T1)) = list(T2).
:- func plus1(int) = int.
:- func makeString(int) = poly_type.
:- pred myFilter(pred(T), list(T), list(T), list(T)).
:- mode myFilter(in(pred(in) is semidet), in, out, out) is det.

:- pred myOdd(int).
:- mode myOdd(in) is det.
:- pred myEven(int).
:- mode myEven(in) is det.

myMap(_, []) = [].
myMap(F, [X|Xs]) = [F(X) | myMap(F,Xs)].

myFilter(_, [], [], []).
myFilter(P, [X|Xs], Ys, Zs) :-
  myFilter(P, Xs, Ys0, Zs0),
  ( if P(X)
    then Ys = [X|Ys0], Zs = Zs0
    else Ys = Ys0, Zs = [X|Zs0]
  ).

myEven(0).
myEven(X) :-
  X = Y + 1,
  myOdd(Y).

myOdd(1).
myOdd(X) :-
  X = Y + 1,
  myEven(Y).

plus1(X) = X+1.

makeString(X) =
  i(X).

main(!IO) :-
  Input = [1,2,3],
  io.format("[%d, %d, %d]\n", myMap(makeString, Input), !IO),
  Result = myMap(plus1, Input),
  io.format("[%d, %d, %d]\n", myMap(makeString, Result), !IO),
  myFilter(even, Input, Even, Odd),
  io.format("Even: [%d]\n", myMap(makeString, Even), !IO),
  io.format("Odd:  [%d, %d]\n", myMap(makeString, Odd), !IO).



