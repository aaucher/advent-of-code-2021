:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).

input_file('inputs/day10-input.txt').

% chunk(Opening, Closing).
chunk('(', ')').
chunk('[', ']').
chunk('{', '}').
chunk('<', '>').

eol --> "\n".
eos([], []).

% Note for readers: this is not recursive, chunk on the left is a DCG,
% chunk//1, chunk on the right is a predicate, chunk/2.
chunk(C) --> [C], { chunk(C,_) }.
chunk(C) --> [C], { chunk(_,C) }.

chunks([]) --> [].
chunks([C|Cs]) --> chunk(C), chunks(Cs).

line(Cs) --> chunks(Cs), (eol; call(eos)).

lines([]) --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

input(Lines) :-
    input_file(File),
    phrase_from_file(lines(Lines), File).

length0([]).

parse(Line, Return) :- parser_(Line, [], Return).

% parser_(Line, ExpectedClosingChunks, Return).
parser_([], [], ok).
parser_([], Missing, i(Missing)) :- \+ length0(Missing).
parser_([O|Cs], Expected, R) :- chunk(O,C), parser_(Cs, [C|Expected], R).
parser_([C|Cs], [C|Expected], R) :- chunk(_,C), parser_(Cs, Expected, R).
parser_([C|_], [E|_], e(C)) :- chunk(_,C), C \= E.

scoreA(')', 3).
scoreA(']', 57).
scoreA('}', 1197).
scoreA('>', 25137).

scoreA(ok, 0).
scoreA(i(_), 0).
scoreA(e(C), S) :- scoreA(C, S).

score_total_A([R|Rs], Score) :- maplist(scoreA, [R|Rs], Scores), sum_list(Scores, Score).

problemA(Result) :-
    input(Lines),
    maplist(parse, Lines, Returns),
    score_total_A(Returns, Result).

scoreB(')', 1).
scoreB(']', 2).
scoreB('}', 3).
scoreB('>', 4).

scoreB(ok, 0).
scoreB(e(_), 0).
scoreB(i(Ms), S) :- maplist(scoreB, Ms, Scores), score_multiplier(Scores, S).

score_multiplier(Scores, Score) :- score_multiplier_(Scores, 0, Score).
score_multiplier_([], Score, Score).
score_multiplier_([S|Scores], Score0, Score) :-
    Score1 #= Score0 * 5 + S,
    score_multiplier_(Scores, Score1, Score).

middle_index(Ls, Mid) :- length(Ls, N), Mid #= N div 2.
middle_element(Ls, L) :- middle_index(Ls, Mid), nth0(Mid, Ls, L).

score_total_B([R|Rs], Score) :-
    maplist(scoreB, [R|Rs], Scores0),
    sort(Scores0, Scores),
    middle_element(Scores, Score).

problemB(Result) :-
    input(Lines),
    maplist(parse, Lines, Returns),
    score_total_B(Returns, Result).
