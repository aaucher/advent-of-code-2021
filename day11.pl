:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).

input_file('inputs/day11-input.txt').

eol --> "\n".
eos([], []).

digit(D) --> [D0], { char_type(D0, decimal_digit), number_codes(D, [D0]) }.

octopus(O) --> digit(O).

octopi([]) --> call(eos), !.
octopi([O|Oi]) --> octopus(O), octopi(Oi).
octopi([O|Oi]) --> octopus(O), eol, octopi(Oi).

make_pair(A,B,A-B).

input(Octopi) :-
    input_file(File),
    phrase_from_file(octopi(Octopi0), File),
    numlist(0, 99, Idx),
    maplist(make_pair, Idx, Octopi0, Octopi).

grid_side(10).
grid_size(100).

state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

inc_energy(N, I-E0, I-E) :- E #= E0 + N.

reset_energy(I-E, I-0) :- E #> 9.
reset_energy(I-E, I-E) :- E #=< 9.

step(Octopi0, Octopi, Flashes) :-
    maplist(inc_energy(1), Octopi0, Octopi1),
    step_flash(Octopi1, Octopi2, Flashes),
    maplist(reset_energy, Octopi2, Octopi).

step_flash(Octopi0, Octopi, Flashes) :-
    step_flash_(Octopi0, Octopi, [], _, [], Flashes).

length_at_least_1([_|_]).

step_flash_(Octopi0, Octopi, Increases0, [], Flashes0, Flashes) :-
    phrase(step_octopi(Octopi0, Octopi), [s(Increases0,Flashes0)], [s([],Flashes)]).
step_flash_(Octopi0, Octopi, Increases0, Increases, Flashes0, Flashes) :-
    phrase(step_octopi(Octopi0, Octopi1), [s(Increases0,Flashes0)], [s(Increases1,Flashes1)]),
    length_at_least_1(Increases1),
    step_flash_(Octopi1, Octopi, Increases1, Increases, Flashes1, Flashes).

step_octopi([], []) --> [].
step_octopi([Octopus0|Octopi0], [Octopus|Octopi]) -->
    step_octopus(Octopus0, Octopus),
    step_octopi(Octopi0, Octopi).

step_octopus(Octopus0, Octopus) -->
    state(s(Increases0,Flashes0), s(Increases,Flashes)),
    { increased_level(Octopus0, Octopus, Increases0, Increases1),
      flash_octopus(Octopus, Flashes0, NewIncreases, NewFlashes),
      append(NewIncreases, Increases1, Increases),
      append(NewFlashes, Flashes0, Flashes) }.

member_count(_, [], [], 0).
member_count(X, [X|Xs], Ys, C) :- member_count(X, Xs, Ys, C0), C #= C0 + 1.
member_count(X, [Y|Xs], [Y|Ys], C) :- X \= Y, member_count(X, Xs, Ys, C).

increased_level(I-E0, I-E, Increases0, Increases) :-
    member_count(I, Increases0, Increases, Count),
    inc_energy(Count, I-E0, I-E).

flash_octopus(_-E, _, [], []) :- E #=< 9.
flash_octopus(I-_, Flashes, [], []) :- memberchk(I, Flashes).
flash_octopus(I-E, Flashes, NewIncreases, [I]) :-
    E #> 9,
    \+ memberchk(I, Flashes),
    findall(N, neighbour(I, N), NewIncreases).

on_first_line(I) :- grid_side(Gd), I #< Gd.
on_last_line(I) :- grid_side(Gd), grid_size(Gz), I #>= Gz - Gd, I #< Gz.
on_first_column(I) :- grid_side(Gd), 0 #= I mod Gd.
on_last_column(I) :- grid_side(Gd), Gd - 1 #= I mod Gd.

neighbour(I, Up) :- \+ on_first_line(I), grid_side(Gd), Up #= I - Gd.
neighbour(I, Dw) :- \+ on_last_line(I), grid_side(Gd), Dw #= I + Gd.
neighbour(I, Lf) :- \+ on_first_column(I), Lf #= I - 1.
neighbour(I, Rt) :- \+ on_last_column(I), Rt #= I + 1.
neighbour(I, D) :- \+ on_first_line(I), \+ on_first_column(I), grid_side(Gd),  D #= I - Gd - 1.
neighbour(I, D) :- \+ on_first_line(I), \+ on_last_column(I), grid_side(Gd),  D #= I - Gd + 1.
neighbour(I, D) :- \+ on_last_line(I), \+ on_first_column(I), grid_side(Gd),  D #= I + Gd - 1.
neighbour(I, D) :- \+ on_last_line(I), \+ on_last_column(I), grid_side(Gd),  D #= I + Gd + 1.

steps(N, Octopi, Flashes) :- steps_(N, Octopi, [], Flashes).

steps_(0, _, Flashes, Flashes).
steps_(N0, Octopi0, Acc, Flashes) :-
    N0 #> 0,
    write(N0), write('.'),
    step(Octopi0, Octopi, NewFlashes),
    N #= N0 - 1,
    steps_(N, Octopi, [NewFlashes|Acc], Flashes).

sum_length(Ls, Sum) :- maplist(length, Ls, Lens), sum_list(Lens, Sum).

problemA(Result) :-
    input(Octopi),
    steps(100, Octopi, Flashes),
    sum_length(Flashes, Result).

all_flashes(Octopi, StepN) :- all_flashes_(Octopi, 1, StepN).

all_flashes_(Octopi0, N, N) :-
    step(Octopi0, _, Flashes),
    grid_size(Gz),
    length(Flashes, Gz).
all_flashes_(Octopi0, N0, N) :-
    write(N0), write('.'),
    step(Octopi0, Octopi, Flashes),
    length(Flashes, Len),
    grid_size(Gz),
    Gz #\= Len,
    N1 #= N0 + 1,
    all_flashes_(Octopi, N1, N).

problemB(Result) :-
    input(Octopi),
    all_flashes(Octopi, Result).
