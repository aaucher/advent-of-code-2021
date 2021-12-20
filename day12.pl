:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(pio)).

% This day I used a different approach by asserting the input into the
% the image. Searching is done by taking advantage of Prolog's backtracking.

input_file('inputs/day12-input.txt').

eol --> "\n".
eos([], []).

big(cave(Cave,big)) --> word_(upper, Cs), { atom_chars(Cave, Cs) }.
small(cave(Cave,small)) --> word_(lower, Cs), { atom_chars(Cave, Cs) }.

word_(Type, [C|Cs]) --> call(Type, C), word_(Type, Cs).
word_(Type, [C]) --> call(Type, C).

lower(C) --> [C], { char_type(C, lower) }.
upper(C) --> [C], { char_type(C, upper) }.

vertex(V) --> big(V).
vertex(V) --> small(V).

edge(From-To) --> vertex(From), "-", vertex(To), (eol; call(eos)).

edges([]) --> call(eos), !.
edges([E|Es]) --> edge(E), edges(Es).

input(Edges) :-
    input_file(File),
    phrase_from_file(edges(Edges), File).

:- dynamic(edge/2).

record_edge(cave(From,_)-cave(To,_)) :-
    assertz(edge(From, To)),
    assertz(edge(To, From)).

:- dynamic(big/1).
:- dynamic(small/1).

record_size(cave(Cave,big)) :- assertz(big(Cave)).
record_size(cave(Cave,small)) :- assertz(small(Cave)).

record_sizes(Edges) :-
    pairs_keys_values(Edges, CavesK, CavesV),
    append(CavesK, CavesV, Caves0),
    sort(Caves0, Caves),
    maplist(record_size, Caves).

record(Edges) :-
    maplist(record_edge, Edges),
    record_sizes(Edges).

cave_check_A(From, Path) :- small(From), \+ memberchk(From, Path).
cave_check_A(From, _) :- big(From).

path(From, To, Path, CaveCheckFn) :- path_(From, To, [], Path, CaveCheckFn).

path_(From, From, Htap, Path, _) :- reverse([From|Htap], Path).
path_(From, To, Path0, Path, CaveCheckFn) :-
    From \= To,
    call(CaveCheckFn, From, Path0),
    edge(From, To0),
    path_(To0, To, [From|Path0], Path, CaveCheckFn).

paths(From, To, Paths, CaveCheckFn) :- findall(P, path(From, To, P, CaveCheckFn), Paths).

write_path(Path) :- write(Path), nl.
write_paths(Paths) :- maplist(write_path, Paths).

problemA(Result) :-
    input(Edges),
    record(Edges),
    paths(start, end, Paths, cave_check_A),
    length(Paths, Result).

member_count(_, [], [], 0).
member_count(X, [X|Xs], Ys, C) :- member_count(X, Xs, Ys, C0), C #= C0 + 1.
member_count(X, [Y|Xs], [Y|Ys], C) :- X \= Y, member_count(X, Xs, Ys, C).

no_small_duplicates([]).
no_small_duplicates([X|Xs]) :- small(X), \+ memberchk(X, Xs), no_small_duplicates(Xs).
no_small_duplicates([X|Xs]) :- big(X), no_small_duplicates(Xs).

cave_check_B(From, Path) :- cave_check_A(From, Path).
% Not the best but fast enough.
cave_check_B(From, Path) :-
    small(From),
    From \= start,
    From \= end,
    member_count(From, Path, _, 1),
    no_small_duplicates(Path).

problemB(Result) :-
    input(Edges),
    record(Edges),
    paths(start, end, Paths, cave_check_B),
    length(Paths, Result).
