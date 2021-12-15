:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).

input_file('inputs/day04-input.txt').

ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].

maybe_comma --> ",".
maybe_comma --> [].

eol --> "\n".

natural_number(N) --> number_(Cs), { number_codes(N, Cs) }.
nat(N) --> natural_number(N).

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

eos([], []).

draw(N) --> natural_number(N), maybe_comma.

draws([]) --> eol, !.
draws([D|Ds]) --> draw(D), draws(Ds).

row(row(A,B,C,D,E)) --> nat(A), ws, nat(B), ws, nat(C), ws, nat(D), ws, nat(E), ws.

board(board(numbers(A,B,C,D,E),M)) --> row(A), row(B), row(C), row(D), row(E), { empty_marks(M) }.

boards([]) --> call(eos), !.
boards([B|Bs]) --> board(B), boards(Bs).

bingo(Draws,Boards) --> draws(Draws), ws, boards(Boards).

input(Draws,Boards) :-
    input_file(File),
    phrase_from_file(bingo(Draws,Boards), File).

empty_row(row(o,o,o,o,o)).
empty_marks(marks(R,R,R,R,R)) :- empty_row(R).

full_row(row(x,x,x,x,x)).
full_marks(marks(R,R,R,R,R)) :- full_row(R).

complete_row(row(x,x,x,x,x)).

complete_col(marks(row(x,_,_,_,_),
		   row(x,_,_,_,_),
		   row(x,_,_,_,_),
		   row(x,_,_,_,_),
		   row(x,_,_,_,_))).
complete_col(marks(row(_,x,_,_,_),
		   row(_,x,_,_,_),
		   row(_,x,_,_,_),
		   row(_,x,_,_,_),
		   row(_,x,_,_,_))).
complete_col(marks(row(_,_,x,_,_),
		   row(_,_,x,_,_),
		   row(_,_,x,_,_),
		   row(_,_,x,_,_),
		   row(_,_,x,_,_))).
complete_col(marks(row(_,_,_,x,_),
		   row(_,_,_,x,_),
		   row(_,_,_,x,_),
		   row(_,_,_,x,_),
		   row(_,_,_,x,_))).
complete_col(marks(row(_,_,_,_,x),
		   row(_,_,_,_,x),
		   row(_,_,_,_,x),
		   row(_,_,_,_,x),
		   row(_,_,_,_,x))).

winning_board(board(_,marks(R,_,_,_,_))) :- complete_row(R).
winning_board(board(_,marks(_,R,_,_,_))) :- complete_row(R).
winning_board(board(_,marks(_,_,R,_,_))) :- complete_row(R).
winning_board(board(_,marks(_,_,_,R,_))) :- complete_row(R).
winning_board(board(_,marks(_,_,_,_,R))) :- complete_row(R).
winning_board(board(_,Marks)) :- complete_col(Marks).

coord(Numbers,Draw,Row,Col) :- coord_row_(Numbers,Draw,Row,Col).

coord_row_(numbers(R,_,_,_,_),D,1,Col) :- coord_col_(R,D,Col).
coord_row_(numbers(_,R,_,_,_),D,2,Col) :- coord_col_(R,D,Col).
coord_row_(numbers(_,_,R,_,_),D,3,Col) :- coord_col_(R,D,Col).
coord_row_(numbers(_,_,_,R,_),D,4,Col) :- coord_col_(R,D,Col).
coord_row_(numbers(_,_,_,_,R),D,5,Col) :- coord_col_(R,D,Col).

coord_col_(row(D,_,_,_,_),D,1).
coord_col_(row(_,D,_,_,_),D,2).
coord_col_(row(_,_,D,_,_),D,3).
coord_col_(row(_,_,_,D,_),D,4).
coord_col_(row(_,_,_,_,D),D,5).

mark(Unmarked,Row,Col,Marked) :- mark_row_(Row,Unmarked,Col,Marked).

mark_row_(1,marks(A,B,C,D,E),Col,marks(Am,B,C,D,E)) :- mark_col_(Col,A,Am).
mark_row_(2,marks(A,B,C,D,E),Col,marks(A,Bm,C,D,E)) :- mark_col_(Col,B,Bm).
mark_row_(3,marks(A,B,C,D,E),Col,marks(A,B,Cm,D,E)) :- mark_col_(Col,C,Cm).
mark_row_(4,marks(A,B,C,D,E),Col,marks(A,B,C,Dm,E)) :- mark_col_(Col,D,Dm).
mark_row_(5,marks(A,B,C,D,E),Col,marks(A,B,C,D,Em)) :- mark_col_(Col,E,Em).

mark_col_(1,row(o,B,C,D,E),row(x,B,C,D,E)).
mark_col_(2,row(A,o,C,D,E),row(A,x,C,D,E)).
mark_col_(3,row(A,B,o,D,E),row(A,B,x,D,E)).
mark_col_(4,row(A,B,C,o,E),row(A,B,C,x,E)).
mark_col_(5,row(A,B,C,D,o),row(A,B,C,D,x)).

mark_board(D,board(Numbers,Unmarked),board(Numbers,Marked)) :-
    coord(Numbers,D,Row,Col),
    mark(Unmarked,Row,Col,Marked).
mark_board(D,board(Numbers,Unmarked),board(Numbers,Unmarked)) :-
    \+ coord(Numbers,D,_,_).

mark_boards(_,[],[]).
mark_boards(D,[B|Bs],[Bm|Bms]) :-
    mark_board(D,B,Bm),
    mark_boards(D,Bs,Bms).

winners([],[],[]).
winners([B|Bs],[B|Winners],Losers) :- winning_board(B), winners(Bs,Winners,Losers).
winners([B|Bs],Winners,[B|Losers]) :- \+ winning_board(B), winners(Bs,Winners,Losers).

play([D|Ds],Boards,Winners,Rest) :-
    mark_boards(D,Boards,Boards1),
    winners(Boards1,[],_),
    play(Ds,Boards1,Winners,Rest).
play([D|Ds],Boards,Winners,[D|Ds]) :-
    mark_boards(D,Boards,Boards1),
    winners(Boards1,Winners,_),
    Winners \= [].

winner(Draws,Boards,Winner,WinningDraw) :- play(Draws,Boards,[Winner|_],[WinningDraw|_]).

sum_unmarked_num(N,o,N).
sum_unmarked_num(_,x,0).

sum_unmarked_row(row(An,Bn,Cn,Dn,En),row(Am,Bm,Cm,Dm,Em),Sum) :-
    maplist(sum_unmarked_num, [An,Bn,Cn,Dn,En], [Am,Bm,Cm,Dm,Em], Nums),
    sum_list(Nums, Sum).

sum_unmarked(board(numbers(An,Bn,Cn,Dn,En),marks(Am,Bm,Cm,Dm,Em)),Sum) :-
    maplist(sum_unmarked_row, [An,Bn,Cn,Dn,En], [Am,Bm,Cm,Dm,Em], Nums),
    sum_list(Nums, Sum).

problemA(Result) :-
    input(Draws,Boards),
    winner(Draws,Boards,Winner,WinningDraw),
    sum_unmarked(Winner, Sum),
    Result #= Sum * WinningDraw.

play_all_draws(Draws,Boards,Winners,Plays) :-
    play_all_draws_(Draws,Boards,[],Winners,[],Plays).

play_all_draws_(_,[],AccW,AccW,AccP,AccP).
play_all_draws_([],_,AccW,AccW,AccP,AccP).
play_all_draws_([D|Ds],Boards,AccW,Winners,AccP,Plays) :-
    mark_boards(D,Boards,Boards1),
    winners(Boards1,NewWinners,Losers),
    play_all_draws_(Ds,Losers,[NewWinners|AccW],Winners,[D|AccP],Plays).

problemB(Result) :-
    input(Draws,Boards),
    play_all_draws(Draws,Boards,[[Winner|_]|_],[WinningDraw|_]),
    sum_unmarked(Winner, Sum),
    Result #= Sum * WinningDraw.

% ?- input(Draws,Boards), play_all_draws(Draws,Boards,[[Winner|_]],_).
