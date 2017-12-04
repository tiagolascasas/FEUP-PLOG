:- use_module(libary(clpfd)).

%test:
%V = [[V1],[V2, V3],[V4, V5, V6],[V7, V8, V9, V10],[V11, V12, V13, V14, V15]]
%T = [[V2, V4, V5, 12],[V4, V7, V8, 9],[V7, V11, V12, 6],[],[]]
%R = 5

trid(N, R) :-
		generateTrid(N, R, V, T),
		solveTrid(V, T, R),
		printTrid(V, T).


solveTrid(V, T, R) :-
		setDomain(V),
		horizontal(V),
		diagonalsRight(V),
		diagonalsLeft(V),
		innerSums(V, T),
		flatten(V, Vf)
		labeling([], Vf).
		
setDomain([], _).
setDomain([L|Ls], R) :-
		domain(L, 1, R),
		setDomain(Ls, R).
		
horizontal([]).
horizontal([L|Lr]) :-
		all_distinct(L),
		horizontal(Lr).
		
diagonalRight(V) :- diagonalRight(V, 1).
diagonalRight([], _).
diagonalRight([L|Ls], N) :- fail.

diagonalLeft(V) :- diagonalLeft(V, 1).
diagonalLeft([], _).
diagonalLeft([L|Ls], N) :- fail.
		
flatten(List, Flat) :- flatten(List, Flat, []).
flatten([], Flat, Flat).
flatten([X|Xs], Flat, Acc) :-
		append(Acc, X, App),
		flatten(Xs, Flat, App).