:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).

input_file('inputs/day02-input.txt').

natural_number(N) --> number_(Cs), { number_codes(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

directive(down) --> "down".
directive(forward) --> "forward".
directive(up) --> "up".

ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].

eos([], []).

entry(command(D,I)) --> directive(D), ws, natural_number(I), ws.

entries([]) --> call(eos), !.
entries([E|Es]) --> entry(E), entries(Es).

input(Es) :-
    input_file(File),
    phrase_from_file(entries(Es), File).

position(Commands,Horiz,Depth) :- position_(Commands, 0, 0, Horiz, Depth).

position_([], H, D, H, D).
position_([command(down,I)|Cs], AccH, AccD, H, D) :-
    AccD1 #= AccD + I,
    position_(Cs, AccH, AccD1, H, D).
position_([command(forward,I)|Cs], AccH, AccD, H, D) :-
    AccH1 #= AccH + I,
    position_(Cs, AccH1, AccD, H, D).
position_([command(up,I)|Cs], AccH, AccD, H, D) :-
    AccD1 #= AccD - I,
    position_(Cs, AccH, AccD1, H, D).

problemA(Result) :-
    input(Commands),
    position(Commands, Horiz, Depth),
    Result #= Horiz * Depth.

position_aim(Commands,Horiz,Depth) :- position_aim_(Commands, 0, 0, 0, Horiz, Depth).

position_aim_([], _, H, D, H, D).
position_aim_([command(down,I)|Cs], Aim, AccH, AccD, H, D) :-
    Aim1 #= Aim + I,
    position_aim_(Cs, Aim1, AccH, AccD, H, D).
position_aim_([command(forward,I)|Cs], Aim, AccH, AccD, H, D) :-
    AccH1 #= AccH + I,
    AccD1 #= AccD + Aim * I,
    position_aim_(Cs, Aim, AccH1, AccD1, H, D).
position_aim_([command(up,I)|Cs], Aim, AccH, AccD, H, D) :-
    Aim1 #= Aim - I,
    position_aim_(Cs, Aim1, AccH, AccD, H, D).

problemB(Result) :-
    input(Commands),
    position_aim(Commands, Horiz, Depth),
    Result #= Horiz * Depth.
