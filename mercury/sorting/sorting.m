:- module sorting.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int, string, solutions.

:- pred minMax(int, int, int, int).
:- mode minMax(in, in, out, out) is nondet.
:- mode minMax(out, out, in, in) is nondet.

minMax(X, Y, Min, Max) :-
  X < Y, Min = X, Max = Y ;
  X >= Y, Max = X, Min = Y.
  % (
  %   if X < Y
  %   then Min = X, Max = Y
  %   else Min = Y, Max = X
  % ).

  % Min = min(X, Y),
  % Max = max(X, Y).

:- pred smallest(list(int), int, list(int)).
:- mode smallest(in, out, out) is nondet.
:- mode smallest(out, in, in) is nondet.

smallest([X], X, []).
smallest([H|T], S, L) :-
  L = [Max|L0],
  minMax(H, S0, S, Max),
  smallest(T, S0, L0).

:- func listToString(list(int)) = string.
listToString(Xs) = join_list(", ", map(int_to_string,Xs)).

:- pred smallestHelper(list(int)::in, {int, list(int)}::out) is nondet.
smallestHelper(Xs, Result) :-
  smallest(Xs, Min, Rest),
  Result = {Min, Rest}.

:- pred sorto(list(int), list(int)).
:- mode sorto(in, out) is nondet.
:- mode sorto(out, in) is nondet.
sorto([], []).
sorto(Xs, Sorted) :-
  Sorted = [Min | SortedRest],
  smallest(Xs, Min, Rest),
  sorto(Rest, SortedRest).

:- pred sortoOutIn(list(int), list(int)).
:- mode sortoOutIn(out, in) is nondet.
sortoOutIn(Xs, Sorted) :- sorto(Xs, Sorted).

:- pred sortoInOut(list(int), list(int)).
:- mode sortoInOut(in, out) is nondet.
sortoInOut(Xs, Sorted) :- sorto(Xs, Sorted).

% :- pred sorto1(list(int), list(int)).
% :- mode sorto1(out, in) is nondet.
% sorto1([], []).
% sorto1(Xs, Sorted) :-
%   Sorted = [Min | SortedRest],
%   smallest(Xs, Min, Rest),
%   sorto1(Rest, SortedRest).

:- pred permutations(list(int), list(int)).
:- mode permutations(in, out) is nondet.
permutations(Xs, Perm) :-
  sorto(Xs, Sorted),
  sorto(Perm, Sorted).
  % sorto1(Perm, Sorted). %% Otherwise modes fail to infer correctly.

main(!IO) :-
  % printMinMax(1, 2, !IO),
  % printMinMax(2, 1, !IO),
  % printMinMax(2, 2, !IO),

  printSmallest([1,2,3], !IO),
  printSmallest([3,1,2], !IO),
  printSmallest([2,3,1], !IO),

  printSorted([1,2,3], !IO),
  printSorted([1,3,2], !IO),
  printSorted([3,2,1], !IO),
  printSorted([1,1,1], !IO),

  printSorted([9,8,7,6,5,4,3,2,1], !IO),

  printPerms([1,2,3], !IO),
  printPerms([1,2,3,4,5,6], !IO).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Printers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred printEachSmallest(list({int, list(int)})::in, io::di, io::uo) is det.
printEachSmallest([], !IO).
printEachSmallest([H|T], !IO) :-
  {Min, Rest} = H,
  io.format( "Min:\t%d,\nRest:\t%s\n\n"
           , [i(Min), s(listToString(Rest))]
           , !IO),
  printEachSmallest(T, !IO).

:- pred printSmallest(list(int)::in, io::di, io::uo) is det.
printSmallest(Xs, !IO) :-
  (
    if Xs = []
    then
      io.format("Cannot apply smallest to the empty list", [], !IO)
    else
      io.format( "List:\t%s\n\n", [s(listToString(Xs))], !IO),
      solutions(smallestHelper(Xs), AllSolutions),
      printEachSmallest(AllSolutions, !IO)
  ).

:- pred printEachList(string::in, list(list(int))::in, io::di, io::uo) is det.
printEachList(_, [], !IO).
printEachList(Name, [H|T], !IO) :-
  io.format( "%s:\t%s\n"
           , [s(Name), s(listToString(H))]
           , !IO),
  printEachList(Name, T, !IO).

:- pred printSorted(list(int)::in, io::di, io::uo) is det.
printSorted(Xs, !IO) :-
  io.format("\nList:\t%s\n", [s(listToString(Xs))], !IO),
  solutions(sortoInOut(Xs), AllSolutions),
  printEachList("Sorted", AllSolutions, !IO).

:- pred printPerms(list(int)::in, io::di, io::uo) is det.
printPerms(Xs, !IO) :-
  io.format("\nList:\t%s\n", [s(listToString(Xs))], !IO),
  solutions(permutations(Xs), AllSolutions),
  printEachList("Permutation", AllSolutions, !IO).

% :- pred printMinMax(int::in, int::in, io::di, io::uo) is det.
% printMinMax(X, Y, !IO) :-
%   minMax(X, Y, Min, Max),
%   io.format( "X:\t%d\nY:\t%d\nMin:\t%d\nMax:\t%d\n\n"
%            , [i(X), i(Y), i(Min), i(Max)]
%            , !IO).
