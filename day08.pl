:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(pio)).
:- use_module(library(reif)).

input_file('inputs/day08-input.txt').

eol --> "\n".
eos([], []).

terminator --> eol.
terminator --> call(eos).

space --> " ".

separator --> "|", space.

segment(a) --> "a".
segment(b) --> "b".
segment(c) --> "c".
segment(d) --> "d".
segment(e) --> "e".
segment(f) --> "f".
segment(g) --> "g".

segments([]) --> [].
segments([Segment|Segments]) --> segment(Segment), segments(Segments).

digit(digit(Segments)) --> segments(Segments0), { sort(Segments0, Segments) }.

digits([]) --> [].
digits([Digit]) --> digit(Digit), (eol; call(eos)).
digits([Digit|Digits]) --> digit(Digit), space, digits(Digits).

signal(Digits) --> digits(Digits).

output(Digits) --> digits(Digits).

display(display(signal(Signal), output(Output))) --> signal(Signal), separator, output(Output).

lines([]) --> call(eos), !.
lines([Line|Lines]) --> display(Line), lines(Lines).

input(Lines) :-
    input_file(File),
    phrase_from_file(lines(Lines), File).

display_output(display(_, Output), Output).

output_digits(output(Digits), Digits).

segment_domain("abcdefg").

digit_segments(0, "abcefg").
digit_segments(1, "cf").
digit_segments(2, "acdeg").
digit_segments(3, "acdfg").
digit_segments(4, "bcdf").
digit_segments(5, "abdfg").
digit_segments(6, "abdefg").
digit_segments(7, "acf").
digit_segments(8, "abcdefg").
digit_segments(9, "abcdfg").

number_of_segments(0, 6).
number_of_segments(1, 2).
number_of_segments(2, 5).
number_of_segments(3, 5).
number_of_segments(4, 4).
number_of_segments(5, 5).
number_of_segments(6, 6).
number_of_segments(7, 3).
number_of_segments(8, 7).
number_of_segments(9, 6).

unique_number_of_segments(N) :- number_of_segments(1, N).
unique_number_of_segments(N) :- number_of_segments(4, N).
unique_number_of_segments(N) :- number_of_segments(7, N).
unique_number_of_segments(N) :- number_of_segments(8, N).

digit_unique_number_of_segments(digit(Segments)) :-
    length(Segments, Len),
    unique_number_of_segments(Len).

is_unique_number_of_segments(Digit, 1) :- digit_unique_number_of_segments(Digit).
is_unique_number_of_segments(Digit, 0) :- \+ digit_unique_number_of_segments(Digit).

flatten(Lss, Ls) :- foldl(append, Lss, [], Ls).

problemA(Result) :-
    input(Lines),
    maplist(display_output, Lines, Outputs),
    maplist(output_digits, Outputs, Digitss),
    flatten(Digitss, Digits),
    maplist(is_unique_number_of_segments, Digits, Unique),
    sum_list(Unique, Result).

digit_constraint(digit(Segments), digit_constraint(Segments, PossibleDigits)) :-
    length(Segments, Len),
    bagof(D, number_of_segments(D, Len), PossibleDigits).

segment_constraint(PossibleSegments, S, segment_constraint(S, PossibleSegments)).

segment_constraint_to_pair(segment_constraint(S, PossibleSegments), S-PossibleSegments).

segment_constraints(digit_constraint(Segments, PossibleDigits), SegmentConstraints) :-
    maplist(digit_segments, PossibleDigits, PossibleSegmentss),
    flatten(PossibleSegmentss, PossibleSegments0),
    sort(PossibleSegments0, PossibleSegments1), % deduplicate
    maplist(segment_constraint(PossibleSegments1), Segments, SegmentConstraints).

intersected_segments(Segmentss, Intersection) :-
    segment_domain(Domain),
    foldl(ord_intersection, Segmentss, Domain, Intersection).

intersected_segments_constraint_pairs(S-PossibleSegmentss, S-Intersection) :-
    intersected_segments(PossibleSegmentss, Intersection).

intersected_constraints(SegmentConstraints, IntersectedConstraints) :-
    maplist(segment_constraint_to_pair, SegmentConstraints, Pairs0),
    sort(Pairs0, Pairs1),
    group_pairs_by_key(Pairs1, Pairs),
    maplist(intersected_segments_constraint_pairs, Pairs, IntersectedPairs),
    maplist(segment_constraint_to_pair, IntersectedConstraints, IntersectedPairs).

undecided(segment_constraint(_, PossibleSegments)) :-
    \+ length0(PossibleSegments),
    \+ length1(PossibleSegments).

is_undecided(SegmentConstraint, true) :- undecided(SegmentConstraint).
is_undecided(SegmentConstraint, false) :- \+ undecided(SegmentConstraint).

select_smallest(segment_constraint(_, Curr),
		segment_constraint(S, SoFar),
		segment_constraint(S, SoFar)) :-
    length(Curr, LenC),
    length(SoFar, LenSF),
    LenC #> LenSF.

select_smallest(segment_constraint(C, Curr),
			  segment_constraint(_, SoFar),
			  segment_constraint(C, Curr)) :-
    length(Curr, LenC),
    length(SoFar, LenSF),
    LenC #=< LenSF.

same_length_possibilities(segment_constraint(_, Segments0), segment_constraint(_, Segments1), true) :-
    same_length(Segments0, Segments1).
same_length_possibilities(segment_constraint(_, Segments0), segment_constraint(_, Segments1), false) :-
    \+ same_length(Segments0, Segments1).

smallest_undecided(SegmentConstraints0, SmallestUndecided) :-
    tfilter(is_undecided, SegmentConstraints0, SegmentConstraints),
    [First|Rest] = SegmentConstraints,
    foldl(select_smallest, Rest, First, SmallestUndecided0),
    tfilter(same_length_possibilities(SmallestUndecided0), SegmentConstraints, SmallestUndecided1),
    member(SmallestUndecided, SmallestUndecided1).

decide(segment_constraint(S, PossibleSegments), SegmentConstraints0, SegmentConstraints) :-
    member(Choice, PossibleSegments),
    decide_(SegmentConstraints0, segment_constraint(S, [Choice]), SegmentConstraints).

decide_([], _, []).
decide_([segment_constraint(St, _)|SegmentConstraints0],
	segment_constraint(St, Decision),
	[segment_constraint(St, Decision)|SegmentConstraints]) :-
    decide_(SegmentConstraints0, segment_constraint(St, Decision), SegmentConstraints).
decide_([segment_constraint(S0, Ps0)|SegmentConstraints0],
	segment_constraint(St, Decision),
	[segment_constraint(S0, Ps)|SegmentConstraints]) :-
    St \= S0,
    ord_subtract(Ps0, Decision, Ps),
    decide_(SegmentConstraints0, segment_constraint(St, Decision), SegmentConstraints).

propagate(SegmentConstraints, SegmentConstraints) :- decode_done(SegmentConstraints).
propagate(SegmentConstraints0, Result) :-
    smallest_undecided(SegmentConstraints0, SmallestUndecided),
    decide(SmallestUndecided, SegmentConstraints0, SegmentConstraints),
    propagate(SegmentConstraints, Result).

length0([]).
length1([_]).

remaining_possibilities(segment_constraint(_, PossibleSegments), N) :-
    length(PossibleSegments, N).

one_possibility_remaining(segment_constraint(_, PossibleSegments)) :- length1(PossibleSegments).

decode_done(SegmentConstraints) :- maplist(one_possibility_remaining, SegmentConstraints).

decoded_mapping(display(signal(Signal), output(Output)), Mapping) :-
    append(Output, Signal, Digits),
    maplist(digit_constraint, Digits, DigitConstraints),
    maplist(segment_constraints, DigitConstraints, Table0),
    flatten(Table0, Table1),
    intersected_constraints(Table1, Table2),
    propagate(Table2, Wiring),
    segment_constraints_to_pairs(Wiring, Mapping).

segment_constraints_to_pairs(SegmentConstraints, Mapping) :-
    maplist(segment_constraint_to_pair, SegmentConstraints, Pairs0),
    group_pairs_by_key(Mapping, Pairs0). % unwrap value from list

decoded_output(Mapping, output(Output), DecodedDigits) :-
    maplist(decoded_digit(Mapping), Output, DecodedDigits).

decoded_digit(Mapping, digit(Segments), DecodedDigit) :-
    maplist(decoded_segment(Mapping), Segments, DecodedSegments0),
    sort(DecodedSegments0, DecodedSegments),
    digit_segments(DecodedDigit, DecodedSegments).

decoded_segment(Mapping, Segment, DecodedSegment) :-
    member(Segment-DecodedSegment, Mapping).

number_list_number([D1,D2,D3,D4], Result) :-
    Result #= D1 * 1000 + D2 * 100 + D3 * 10 + D4.

sum_digit_lists(DigitLists, Result) :-
    maplist(number_list_number, DigitLists, Ints),
    sum_list(Ints, Result).

decode(Display, DecodedDigits) :-
    decoded_mapping(Display, Mapping),
    display_output(Display, Output),
    decoded_output(Mapping, Output, DecodedDigits).

% debug
decode_all(Lines, Result) :- decode_all_(Lines, [], Result).

decode_all_([], Acc, Acc).
decode_all_([L|Ls], Acc, Result) :-
    decode(L, DecodedDigits),
    write(DecodedDigits),
    !,
    decode_all_(Ls, [DecodedDigits|Acc], Result).

problemB(Result) :-
    input(Lines),
    maplist(decode, Lines, DigitLists),
    sum_digit_lists(DigitLists, Result).
