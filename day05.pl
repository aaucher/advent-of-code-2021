:- use_module(library(assoc)).
:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).

input_file('inputs/day05-input.txt').

ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].

arrow --> "->".
comma --> ",".

nat(N) --> number_(Cs), { number_codes(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

point(point(X,Y)) --> nat(X), comma, nat(Y).

segment(segment(A,B)) --> point(A), ws, arrow, ws, point(B), ws.

eos([], []).

segments([]) --> eos, !.
segments([S|Ss]) --> segment(S), segments(Ss).

input(Segments) :-
    input_file(File),
    phrase_from_file(segments(Segments), File).

print_segment(S) :- write(S), nl.
print_segments(Segments) :- maplist(print_segment, Segments).

horizontal(segment(point(X1,Y),point(X2,Y))) :- X1 #\= X2.
vertical(segment(point(X,Y1),point(X,Y2))) :- Y1 #\= Y2.
diagonal(segment(point(X1,Y1),point(X2,Y2))) :- % for problem B
    X1 #\= X2,
    Y1 #\= Y2,
    X #= X2 - X1,
    Y #= Y2 - Y1,
    (X #= Y; X #= -Y).

normalise(segment(A,B),segment(A,B)) :- ordered(A,B).
normalise(segment(A,B),segment(B,A)) :- \+ ordered(A,B).

ordered(point(X1,Y1),point(X2,Y2)) :- X1 #=< X2, Y1 #=< Y2.

points_in_segment(S, Points) :-
    horizontal(S),
    normalise(S, S1),
    segment(point(X1,Y),point(X2,Y)) = S1,
    points_from_to_x(X1, X2, Y, Points).
points_in_segment(S, Points) :-
    vertical(S),
    normalise(S, S1),
    segment(point(X,Y1),point(X,Y2)) = S1,
    points_from_to_y(Y1, Y2, X, Points).
points_in_segment(S, Points) :-  % for problem B
    diagonal(S),
    normalise(S, S1),
    segment(point(X1,Y1),point(X2,Y2)) = S1,
    points_from_to_d(X1, X2, Y1, Y2, Points).
points_in_segment(segment(A,B), [A]) :- A = B.
points_in_segment(S, []) :-
    \+ horizontal(S),  % for problem B
    \+ vertical(S),
    \+ diagonal(S),
    segment(point(X1,Y1),point(X2,Y2)) = S,
    X1 #\= X2,
    Y1 #\= Y2.

make_point_xy(X,Y,point(X,Y)).
make_point_yx(Y,X,point(X,Y)).

points_from_to_x(Xa,Xb,Y,Points) :-
    numlist(Xa,Xb,Xs),
    maplist(make_point_yx(Y),Xs,Points).
points_from_to_y(Ya,Yb,X,Points) :-
    numlist(Ya,Yb,Ys),
    maplist(make_point_xy(X),Ys,Points).

 % for problem B
points_from_to_d(Xa,Xb,Ya,Yb,Points) :- Xa #< Xb, Ya #< Yb, points_from_to_d_(Xa,Xb,Ya,Yb,Points).
points_from_to_d(Xa,Xb,Ya,Yb,Points) :- Xa #> Xb, Ya #> Yb, points_from_to_d_(Xb,Xa,Yb,Ya,Points).
points_from_to_d(Xa,Xb,Ya,Yb,Points) :- Xa #< Xb, Ya #> Yb, points_from_to_d_revy(Xa,Xb,Yb,Ya,Points).
points_from_to_d(Xa,Xb,Ya,Yb,Points) :- Xa #> Xb, Ya #< Yb, points_from_to_d_revx(Xb,Xa,Ya,Yb,Points).

points_from_to_d_(Xa,Xb,Ya,Yb,Points) :-
    numlist(Xa,Xb,Xs),
    numlist(Ya,Yb,Ys),
    maplist(make_point_xy,Xs,Ys,Points).
points_from_to_d_revx(Xa,Xb,Ya,Yb,Points) :-
    numlist(Xa,Xb,Xs),
    reverse(Xs,Sx),
    numlist(Ya,Yb,Ys),
    maplist(make_point_xy,Sx,Ys,Points).
points_from_to_d_revy(Xa,Xb,Ya,Yb,Points) :-
    numlist(Xa,Xb,Xs),
    numlist(Ya,Yb,Ys),
    reverse(Ys,Sy),
    maplist(make_point_xy,Xs,Sy,Points).

coverage(Segments, Coverage) :-
    empty_assoc(Assoc),
    foldl(put_segment, Segments, Assoc, Coverage).

put_segment(S, Assoc0, Coverage) :-
    points_in_segment(S, Points),
    foldl(put_point, Points, Assoc0, Coverage).

put_point(Point, Assoc0, Assoc) :-
    \+ get_assoc(Point, Assoc0, _),
    put_assoc(Point, Assoc0, 1, Assoc).
put_point(Point, Assoc0, Assoc) :-
    get_assoc(Point, Assoc0, Value0),
    Value #= Value0 + 1,
    put_assoc(Point, Assoc0, Value, Assoc).

filter_by_weight(Criteria, Coverage, Filtered) :-
    assoc_to_list(Coverage,Pairs),
    foldl(Criteria,Pairs,[],Filtered).

weight_two_or_more(Point-Weight, Acc, [Point-Weight|Acc]) :- Weight #>= 2.
weight_two_or_more(_-Weight, Acc, Acc) :- Weight #< 2.

% To solve problem A, run without diagonal handling.
problem(Result) :-
    input(Segments),
    coverage(Segments, Coverage),
    filter_by_weight(weight_two_or_more, Coverage, Smog),
    length(Smog, Result).
