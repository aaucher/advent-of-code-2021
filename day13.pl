:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(pio)).

input_file('inputs/day13-input.txt').

comma --> ",".
eol --> "\n".
eos([], []).

nat(N) --> number_(Cs), { number_codes(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

dot(X-Y) --> nat(X), comma, nat(Y).

dots([]) --> [].
dots([D|Ds]) --> dot(D), eol, dots(Ds).

fold(fold(up, Y)) --> "fold along y=", nat(Y).
fold(fold(left, X)) --> "fold along x=", nat(X).

folds([]) --> [].
folds([F|Fs]) --> fold(F), (eol; call(eos)), folds(Fs).

instructions(Dots, Folds) --> dots(Dots), eol, folds(Folds).

input(Dots, Folds) :-
    input_file(File),
    phrase_from_file(instructions(Dots, Folds), File).

one_fold(fold(up, _), [], []).
one_fold(fold(up, Yf), [X-Y|Dots0], [X-Y|Dots]) :-
    Y #=< Yf,
    one_fold(fold(up, Yf), Dots0, Dots).
one_fold(fold(up, Yf), [X-Y0|Dots0], [X-Y|Dots]) :-
    Y0 #> Yf,
    Y #= Yf - (Y0 - Yf),
    one_fold(fold(up, Yf), Dots0, Dots).

one_fold(fold(left, _), [], []).
one_fold(fold(left, Xf), [X-Y|Dots0], [X-Y|Dots]) :-
    X #=< Xf,
    one_fold(fold(left, Xf), Dots0, Dots).
one_fold(fold(left, Xf), [X0-Y|Dots0], [X-Y|Dots]) :-
    X0 #> Xf,
    X #= Xf - (X0 - Xf),
    one_fold(fold(left, Xf), Dots0, Dots).

no_duplicates(Xs, Ys) :- sort(Xs, Ys).

problemA(Result) :-
    input(Dots0, [Fold|_]),
    one_fold(Fold, Dots0, Dots1),
    no_duplicates(Dots1, Dots),
    length(Dots, Result).

many_folds([], Dots, Dots).
many_folds([Fold|Folds], Dots0, Dots) :-
    one_fold(Fold, Dots0, Dots1),
    no_duplicates(Dots1, Dots2),
    many_folds(Folds, Dots2, Dots).

% Copied from scryer prolog library(lists).
% Those predicates are supposed to be exported by library(lists) but I get the
% following error:
% error(existence_error(procedure,list_min/2),list_min/2)
% This code is under a BSD 3-Clause License.
list_max([N|Ns], Max) :-
    foldl(list_max_, Ns, N, Max).

list_max_(N, Max0, Max) :-
    Max is max(N, Max0).

list_min([N|Ns], Min) :-
    foldl(list_min_, Ns, N, Min).

list_min_(N, Min0, Min) :-
    Min is min(N, Min0).
% End of section copied from scryer prolog.

on_first_column(ColN, X) :- 0 #= X mod ColN.

maybe_write_nl(ColN, P) :- on_first_column(ColN, P), nl.
maybe_write_nl(ColN, P) :- \+ on_first_column(ColN, P).

draw_pixels(_, [], _).
draw_pixels(ColN, [P|Ps], [P|Ds]) :-
    maybe_write_nl(ColN, P),
    write('#'),
    draw_pixels(ColN, Ps, Ds).
draw_pixels(ColN, [P|Ps], [D|Ds]) :-
    P \= D,
    maybe_write_nl(ColN, P),
    write(' '),
    draw_pixels(ColN, Ps, [D|Ds]).

coord_to_index(ColN, X-Y, Idx) :- Idx #= X + ColN * Y.

grid_size(Dots, ColN, RowN) :-
    pairs_keys_values(Dots, Xs, Ys),
    list_max(Xs, MaxX),
    list_max(Ys, MaxY),
    ColN #= MaxX + 1,
    RowN #= MaxY + 1.

pixels(ColN, RowN, Pixels) :-
    coord_to_index(ColN, ColN-RowN, Last),
    numlist(0, Last, Pixels).

display(Dots) :-
    grid_size(Dots, ColN, RowN),
    pixels(ColN, RowN, Pixels),
    maplist(coord_to_index(ColN), Dots, DotIndexes0),
    sort(DotIndexes0, DotIndexes),
    draw_pixels(ColN, Pixels, DotIndexes).

problemB :-
    input(Dots0, Folds),
    many_folds(Folds, Dots0, Dots),
    display(Dots).
