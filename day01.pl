:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).

input_file('inputs/day01-input.txt').

natural_number(N) --> number_(Cs), { number_codes(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

eol --> "\n".
eos([],[]).

measurement(N) --> natural_number(N).

measurements([]) --> call(eos), !.
measurements([N|Ns]) --> measurement(N), (eol; eos), measurements(Ns).

input(Xs) :-
    input_file(File),
    phrase_from_file(measurements(Xs), File).

increased(A, B) :- A #< B.

number_of_increased(Xs, N) :- number_of_increased_(Xs, 0, N).

number_of_increased_([First,Next|Xs], Acc, N) :-
    increased(First, Next),
    Acc1 #= Acc + 1,
    number_of_increased_([Next|Xs], Acc1, N).

number_of_increased_([First,Next|Xs], Acc, N) :-
    \+ increased(First, Next),
    number_of_increased_([Next|Xs], Acc, N).

number_of_increased_([_], N, N).

problemA(Result) :-
    input(Xs),
    number_of_increased(Xs, Result).

increased_3tuple(X1, X2, X3, X4) :-
    A #= X1 + X2 + X3,
    B #= X2 + X3 + X4,
    A #< B.

number_of_increased_3tuple(Xs, N) :- number_of_increased_3tuple_(Xs, 0, N).

number_of_increased_3tuple_([X1,X2,X3,X4|Xs], Acc, N) :-
    increased_3tuple(X1, X2, X3, X4),
    Acc1 #= Acc + 1,
    number_of_increased_3tuple_([X2,X3,X4|Xs], Acc1, N).

number_of_increased_3tuple_([X1,X2,X3,X4|Xs], Acc, N) :-
    \+ increased_3tuple(X1,X2,X3,X4),
    number_of_increased_3tuple_([X2,X3,X4|Xs], Acc, N).

number_of_increased_3tuple_([_,_,_], N, N).

problemB(Result) :-
    input(Xs),
    number_of_increased_3tuple(Xs, Result).
