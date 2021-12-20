:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(pio)).

input_file('inputs/day14-input.txt').

eol --> "\n".
eos([], []).

element(C) --> [C], { char_type(C, upper) }.

polymer([C|Cs]) --> element(C), polymer(Cs).
polymer([C]) --> element(C).

element_pair(A, B) --> element(A), element(B).

pair_insertion(p(A, B, C)) --> element_pair(A, B), " -> ", element(C).

pair_insertions([]) --> [].
pair_insertions([P|Ps]) --> pair_insertion(P), (eol; call(eos)), pair_insertions(Ps).

polymerization(Template, Pairs) --> polymer(Template), eol, eol, pair_insertions(Pairs).

input(Template, Pairs) :-
    input_file(File),
    phrase_from_file(polymerization(Template, Pairs), File).

:- dynamic(insertion/3).

record_insertion(p(A,B,C)) :- assertz(insertion(A,B,C)).

record_insertions(Pairs) :- maplist(record_insertion, Pairs).

polymer_to_pairs([E], [E-'.'-1]).
polymer_to_pairs([E0,E1|Polymer], [E0-E1-1|Pairs]) :- polymer_to_pairs([E1|Polymer], Pairs).

sum_values(E-Counts, E-C) :- sum_list(Counts, C).

step(Pairs0, Pairs) :- step_(Pairs0, [], Pairs).

step_([], Pairs0, Pairs) :-
    keysort(Pairs0, Pairs1),
    group_pairs_by_key(Pairs1, Pairs2),
    maplist(sum_values, Pairs2, Pairs).
step_([L-R-C|Pairs0], Acc, Pairs) :-
    R \= '.',
    insertion(L, R, M),
    step_(Pairs0, [L-M-C,M-R-C|Acc], Pairs).
step_([L-'.'-C|Pairs0], Acc, Pairs) :-
    step_(Pairs0, [L-'.'-C|Acc], Pairs).

steps(N, Pairs0, Pairs) :- steps_(N, Pairs0, Pairs).

steps_(0, Pairs, Pairs).
steps_(N0, Pairs0, Pairs) :-
    N0 #> 0,
    step(Pairs0, Pairs1),
    N #= N0 - 1,
    steps_(N, Pairs1, Pairs).

pair_first_element_count(E0-_-Count, E0-Count).

occurrences(Pairs0, Occurrences) :-
    maplist(pair_first_element_count, Pairs0, Pairs1),
    keysort(Pairs1, Pairs2),
    group_pairs_by_key(Pairs2, Pairs),
    maplist(sum_values, Pairs, Occurrences).

swapped_pair(K-V, V-K).

last([X], X).
last([_|Rest], X) :- last(Rest, X).

score(Occurrences0, Score) :-
    maplist(swapped_pair, Occurrences0, Occurrences1),
    sort(Occurrences1, Occurrences),
    nth0(0, Occurrences, Min-_),
    last(Occurrences, Max-_),
    Score #= Max - Min.

problem(StepN, Result) :-
    input(Template, Rules),
    record_insertions(Rules),
    polymer_to_pairs(Template, Pairs0),
    steps(StepN, Pairs0, Pairs),
    occurrences(Pairs, Occurrences),
    score(Occurrences, Result).

problemA(Result) :- problem(10, Result).

problemB(Result) :- problem(40, Result).
