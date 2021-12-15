:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).

input_file('inputs/day09-input.txt').

% For efficent access to points, the heightmap is stored in an assoc.
% Each point is indexed by its coordinates.

eol --> "\n".
eos([], []).

digit(D) --> [D0], { char_type(D0, decimal_digit), number_codes(D, [D0]) }.

point(P) --> digit(P).

points([]) --> [].
points([p(P)|Ps]) --> point(P), points(Ps).

row(r(Ps)) --> points(Ps), (eol; call(eos)).

rows([]) --> call(eos), !.
rows([R|Rs]) --> row(R), rows(Rs).

heightmap(raw_hm(Rs)) --> rows(Rs).

input(Heightmap) :-
    input_file(File),
    phrase_from_file(heightmap(Heightmap), File).

state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

% Process raw heightmap using a DCG and semicontext notation.
% Heightmap is an assoc c(X,Y)-Height
indexed_heightmap(raw_hm(RawHeightmap), hm(Heightmap,Rn,Cn)) :-
    empty_assoc(Heightmap0),
    phrase(indexed_heightmap_(RawHeightmap), [s(-1,-1,Heightmap0)], [s(Cn,Rn,Heightmap)]).

indexed_heightmap_([]) --> [].
indexed_heightmap_([L|Ls]) --> indexed_heightmap_(L), indexed_heightmap_(Ls).
indexed_heightmap_(r(Ps)) -->
    state(s(_,Ri0,Heightmap), s(Ci,Ri,Heightmap)),
    { Ci #= -1,
      Ri #= Ri0 + 1 },
    indexed_heightmap_(Ps).
indexed_heightmap_(p(P)) -->
    state(s(Ci0,Ri,Heightmap0), s(Ci,Ri,Heightmap)),
    { Ci #= Ci0 + 1,
      put_assoc(c(Ci,Ri), Heightmap0, P, Heightmap) }.

height(Heightmap, c(X,Y), H) :- get_assoc(c(X,Y), Heightmap, H).
height(Heightmap, c(X,Y), 10) :- \+ get_assoc(c(X,Y), Heightmap, _).

% adjacent_points(c(X,Y), Up, Down, Left, Right).
adjacent_points(c(X,Y)-_, Up, Down, Left, Right) :- adjacent_points(c(X,Y), Up, Down, Left, Right).
adjacent_points(c(X,Y), c(X,Yu), c(X,Yd), c(Xl,Y), c(Xr,Y)) :-
    Yu #= Y - 1,
    Yd #= Y + 1,
    Xl #= X - 1,
    Xr #= X + 1.

low_point(H, Up, Down, Left, Right) :- H #< Up, H #< Down, H #< Left, H #< Right.

low_points(hm(Heightmap,_,_), LowPoints) :-
    assoc_to_list(Heightmap, Points),
    foldl(low_points_(Heightmap), Points, [], LowPoints).
low_points_(Heightmap, c(X,Y)-H, Acc0, Acc) :-
    adjacent_points(c(X,Y), Up, Down, Left, Right),
    height(Heightmap, Up, Hu),
    height(Heightmap, Down, Hd),
    height(Heightmap, Left, Hl),
    height(Heightmap, Right, Hr),
    low_points_(c(X,Y)-H, Hu, Hd, Hl, Hr, Acc0, Acc).
low_points_(c(X,Y)-H, Up, Down, Left, Right, Acc0, [c(X,Y)-H|Acc0]) :-
    low_point(H, Up, Down, Left, Right).
low_points_(_-H, Up, Down, Left, Right, Acc0, Acc0) :-
    \+ low_point(H, Up, Down, Left, Right).

risk_level(_-Height, RiskLevel) :- RiskLevel #= Height + 1.

sum_risk_level(LowPoints, Sum) :-
    maplist(risk_level, LowPoints, RiskLevels),
    sum_list(RiskLevels, Sum).

problemA(Result) :-
    input(RawHeightmap),
    indexed_heightmap(RawHeightmap, Heightmap),
    low_points(Heightmap, LowPoints),
    sum_risk_level(LowPoints, Result).

low_altitude(_-H) :- H #< 9.

bassin(hm(Heightmap,_,_), P, Bassin) :-
    bassin_(Heightmap, [P], [], Bassin).

% bassin_(Heightmap, ToVisit, Bassin0,  Bassin).
bassin_(_, [], Acc, Acc).
bassin_(Heightmap, [P|Ps], Acc0, Acc) :-
    \+ memberchk(P, Acc0),
    low_altitude(P),
    adjacent_points(P, Up, Down, Left, Right),
    height(Heightmap, Up, Hu),
    height(Heightmap, Down, Hd),
    height(Heightmap, Left, Hl),
    height(Heightmap, Right, Hr),
    bassin_(Heightmap, [Up-Hu,Down-Hd,Left-Hl,Right-Hr|Ps], [P|Acc0], Acc).
bassin_(Heightmap, [P|Ps], Acc0, Acc) :-
    memberchk(P, Acc0),
    bassin_(Heightmap, Ps, Acc0, Acc).
bassin_(Heightmap, [P|Ps], Acc0, Acc) :-
    \+ low_altitude(P),
    bassin_(Heightmap, Ps, Acc0, Acc).

len_list_pair(Ls, N-Ls) :- length(Ls, N).

scoreB(Bassins, Result) :-
    maplist(len_list_pair, Bassins, LenBassins0),
    keysort(LenBassins0, LenBassins1),
    reverse(LenBassins1, LenBassins),
    [Lf-_,Ls-_,Lt-_|_] = LenBassins,
    Result #= Lf * Ls * Lt.

problemB(Result) :-
    input(RawHeightmap),
    indexed_heightmap(RawHeightmap, Heightmap),
    low_points(Heightmap, LowPoints),
    maplist(bassin(Heightmap), LowPoints, Bassins),
    scoreB(Bassins, Result).
