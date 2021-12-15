:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).

input_file('inputs/day03-input.txt').

ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].

eos([], []).

bit(0) --> "0".
bit(1) --> "1".

number(number([A,B,C,D,E,F,G,H,I,J,K,L])) -->
   bit(A),bit(B),bit(C),bit(D),bit(E),bit(F),bit(G),bit(H),bit(I),bit(J),bit(K),bit(L),ws.

numbers([]) --> call(eos), !.
numbers([N|Ns]) --> number(N), numbers(Ns).

input(Ns) :-
    input_file(File),
    phrase_from_file(numbers(Ns), File).

truth_bit(Goal,1) :- call(Goal), !.
truth_bit(Goal,0) :- \+ call(Goal).

neg(0,1).
neg(1,0).

number_list(number(Ns), Ns).

most_common_bit(Bits,Bit) :-
    length(Bits, Len),
    sum_list(Bits, Sum),
    Half #= Len div 2 + Len rem 2,
    truth_bit(Sum #>= Half, Bit).

least_common_bit(Bits,Bit) :- most_common_bit(Bits, Bit1), neg(Bit1,Bit).

common_bits(_,[],[]).
common_bits(Criteria,[C|Cs],[Bit|Bits]) :-
    call(Criteria, C, Bit),
    common_bits(Criteria, Cs, Bits).

gamma_bin(Numbers,number(Bits)) :-
    maplist(number_list, Numbers, Rows),
    transpose(Rows, Cols),
    common_bits(most_common_bit, Cols, Bits).

epsilon_bin(number(Bits),number(Inv)) :- maplist(neg, Bits, Inv).

powers_of_2([1,2,4,8,16,32,64,128,256,512,1024,2048]).

mul(A,B,R) :- R #= A * B.

bin_int(number(Bits), Int) :-
    powers_of_2(Powers),
    reverse(Powers,Srewop),
    maplist(mul,Bits,Srewop,Ints),
    sum_list(Ints, Int).

power(Numbers,Power) :-
    gamma_bin(Numbers,G),
    epsilon_bin(G,E),
    bin_int(G,Gd),
    bin_int(E,Ed),
    Power #= Gd * Ed.

problemA(Result) :-
    input(Numbers),
    power(Numbers, Result).

oxygen_rating(Numbers,Oxygen) :- rating_(Numbers,most_common_bit,0,Oxygen).
co2_rating(Numbers,CO2) :- rating_(Numbers,least_common_bit,0,CO2).

nth0_bit_is(Idx,Bit,number(Ns),true) :- nth0(Idx, Ns, Bit).
nth0_bit_is(Idx,Bit,number(Ns),false) :- \+ nth0(Idx, Ns, Bit).

rating_([N],_,_,N).
rating_([N|Ns],Criteria,Idx,Oxygen) :-
    maplist(number_list, [N|Ns], Rows),
    maplist(nth0(Idx), Rows, Col),
    call(Criteria, Col, Bit),
    tfilter(nth0_bit_is(Idx, Bit),[N|Ns],Numbers),
    Idx1 #= Idx + 1,
    rating_(Numbers,Criteria,Idx1,Oxygen).

support_rating(Numbers,Support) :-
    oxygen_rating(Numbers,O),
    co2_rating(Numbers,C),
    bin_int(O,Od),
    bin_int(C,Cd),
    Support #= Od * Cd.

problemB(Result) :-
    input(Numbers),
    support_rating(Numbers, Result).
