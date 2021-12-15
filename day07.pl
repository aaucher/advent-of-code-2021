:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).

input_file('inputs/day07-input.txt').

comma --> ",".
eol --> "\n".

separator --> comma.
separator --> call(eos).

nat(N) --> number_(Cs), { number_codes(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

crab(H) --> nat(H), separator.

eos([], []).

crabs([]) --> eos, !.
crabs([C|Cs]) --> crab(C), crabs(Cs).

input(Crabs) :-
    input_file(File),
    phrase_from_file(crabs(Crabs), File).

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

sum_to_int(N,Sum) :- Sum is N*(N+1) div 2.

%distance_difference(Crab,Crab,0).
%distance_difference(Crab,Idx,Diff) :- Crab #\= Idx, Diff #= Idx - Crab.
distance_differenceA(Crab,Idx,Diff) :- Diff is Idx-Crab.
distance_differenceB(Crab,Idx,Diff) :- Diff0 is Idx-Crab, Diff1 is abs(Diff0), sum_to_int(Diff1,Diff).

abs(N, A) :- A is abs(N).

add_abs(A0,B0,R) :- abs(A0,A), abs(B0,B), R is A + B.

indexes(Min,Max,Indexes) :- numlist(Min,Max,Indexes).

distance_difference_list(CostFn,Indexes,Crab,Diff) :-
    maplist(call(CostFn,Crab),Indexes,Diff).

%distance_folder(Indexes,Crab,[LastCrab|Crabs],[crab(Crab,Diffs,Cheapest),LastCrab|Crabs]) :-
distance_folder(CostFn,Indexes,Crab,Cheapest0,Cheapest) :-
    distance_difference_list(CostFn,Indexes,Crab,Diff),
    maplist(add_abs,Diff,Cheapest0,Cheapest).

cheapest_outcome_matrix(CostFn,[Crab0|Crabs],Result) :-
    list_min([Crab0|Crabs],Min),
    list_max([Crab0|Crabs],Max),
    indexes(Min,Max,Indexes),
    distance_difference_list(CostFn,Indexes,Crab0,Diffs0),
    maplist(abs,Diffs0,Cheapest0),
    foldl(distance_folder(CostFn,Indexes),Crabs,Cheapest0,Result).

cheapest_outcome(Consumption,Outcome) :- list_min(Consumption,Outcome).

problemA(Result) :-
    input(Crabs),
    cheapest_outcome_matrix(distance_differenceA,Crabs,Cheapest),
    cheapest_outcome(Cheapest,Result).

problemB(Result) :-
    input(Crabs),
    cheapest_outcome_matrix(distance_differenceB,Crabs,Cheapest),
    cheapest_outcome(Cheapest,Result).

%          x
%2 [ 2, 1, 0, 1, 2, 3, 4, 5, 6, 7]
% R[ 2, 1, 0, 1, 2, 3, 4, 5, 6, 7]
%                   x
%5 [ 5, 4, 3, 2, 1, 0, 1, 2, 3, 4]
% R[ 7, 5, 3, 3, 3, 3, 5, 7, 9,11]
%             x
%3 [ 3, 2, 1, 0, 1, 2, 3, 4, 5, 6]
% R[10, 7, 4, 3, 4, 5, 8,11,14,17]
