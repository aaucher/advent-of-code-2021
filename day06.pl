:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).

input_file('inputs/day06-input.txt').

comma --> ",".
eol --> "\n".

separator --> comma.
separator --> eol.

nat(N) --> number_(Cs), { number_codes(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

lanternfish(T) --> nat(T), separator.

eos([], []).

lanternfishes([]) --> eos, !.
lanternfishes([F|Fs]) --> lanternfish(F), lanternfishes(Fs).

input(Fish) :-
    input_file(File),
    phrase_from_file(lanternfishes(Fish), File).

empty_buckets(buckets(0,0,0,0,0,0,0,0,0)).

fill(0,buckets(B0,B1,B2,B3,B4,B5,B6,B7,B8),buckets(B,B1,B2,B3,B4,B5,B6,B7,B8)) :- B #= B0 + 1.
fill(1,buckets(B0,B1,B2,B3,B4,B5,B6,B7,B8),buckets(B0,B,B2,B3,B4,B5,B6,B7,B8)) :- B #= B1 + 1.
fill(2,buckets(B0,B1,B2,B3,B4,B5,B6,B7,B8),buckets(B0,B1,B,B3,B4,B5,B6,B7,B8)) :- B #= B2 + 1.
fill(3,buckets(B0,B1,B2,B3,B4,B5,B6,B7,B8),buckets(B0,B1,B2,B,B4,B5,B6,B7,B8)) :- B #= B3 + 1.
fill(4,buckets(B0,B1,B2,B3,B4,B5,B6,B7,B8),buckets(B0,B1,B2,B3,B,B5,B6,B7,B8)) :- B #= B4 + 1.
fill(5,buckets(B0,B1,B2,B3,B4,B5,B6,B7,B8),buckets(B0,B1,B2,B3,B4,B,B6,B7,B8)) :- B #= B5 + 1.
fill(6,buckets(B0,B1,B2,B3,B4,B5,B6,B7,B8),buckets(B0,B1,B2,B3,B4,B5,B,B7,B8)) :- B #= B6 + 1.

live_a_day(buckets(B0,B1,B2,B3,B4,B5,B6,B7,B8),buckets(B1,B2,B3,B4,B5,B6,B,B8,B0)) :- B #= B0 + B7.

live(0, Buckets, Buckets).
live(N0, Buckets0, Buckets) :-
    N0 #> 0,
    N #= N0 - 1,
    live_a_day(Buckets0, Buckets1),
    live(N, Buckets1, Buckets).

buckets_list(buckets(B0,B1,B2,B3,B4,B5,B6,B7,B8), [B0,B1,B2,B3,B4,B5,B6,B7,B8]).

number_of_fish(Days, Fish0, N) :-
    empty_buckets(Buckets0),
    foldl(fill, Fish0, Buckets0, Buckets1),
    live(Days, Buckets1, Buckets),
    buckets_list(Buckets, Bs),
    sum_list(Bs, N).

problemA(Result) :-
    input(Fish),
    number_of_fish(80, Fish, Result).

problemB(Result) :-
    input(Fish),
    number_of_fish(256, Fish, Result).
