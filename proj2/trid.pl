:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trid entrypoint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trid(N, R) :-
		generateTrid(N, R, V, T),
		solveTrid(V, T, R),
		printTrid(V, T).

testSolver :-
        V = [[V1],[V2, V3],[V4, V5, V6],[V7, V8, V9, V10],[V11, V12, V13, V14, V15]],
        T = [[V2, V4, V5, 12],[V4, V7, V8, 9],[V7, V11, V12, 6],[V7, V11, V12, 6],[V8, V12, V13, 12],[V9, V13, V14, 8]],
        R = 5,
        solveTrid(V, T, R),
        write(V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trid Solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Trid solver:
% V - list of lists with the vertices
% T - list of liss with the triangles and inner sums
% R - range of the numbers
solveTrid(V, T, R) :-
		setDomain(V, R),
		horizontal(V),
        diagonalsRight(V),
		diagonalsLeft(V),
		innerSums(T),
		flatten(V, Vf),
		labeling([], Vf).

%Sets the domain of all vertices between 1 and the specified value R
setDomain([], _).
setDomain([L|Ls], R) :-
		domain(L, 1, R),
		setDomain(Ls, R).

%Sets each horizontal line of the board to have distinct elements within that line
horizontal([]).
horizontal([L|Lr]) :-
		all_distinct(L),
		horizontal(Lr).

%Sets each "\" diagonal of the board to have distinct elements within that diagonal
diagonalsRight(V) :-
        nl, write(V), nl, nl,
        length(V, N),
        diagonalsRight(V, N).
diagonalsRight(_, 0).
diagonalsRight([V|Vx], N) :-
        restrictRight([V|Vx], N),
        M is N - 1,
        diagonalsRight(Vx, M).

restrictRight(V, N) :- restrictRight(V, N, [], 0).
restrictRight([], _, Diagonal, _) :- all_distinct(Diagonal), write(Diagonal), nl.
restrictRight([X|Xs], N, Acc, R) :-
        element(Rn, X, Elem),
        append(Acc, [Elem], App),
        Rn is R + 1,
        restrictRight(Xs, N, App, Rn).

%Sets each "/" diagonal of the board to have distinct elements within that diagonal
diagonalsLeft(V) :-
        length(V, N),
        diagonalsLeft(V, N).
diagonalsLeft(_, 0).
diagonalsLeft(V, N) :-
        restrictLeft(V, N),
        M is N - 1,
        diagonalsLeft(V, M).

restrictLeft(V, N) :- restrictLeft(V, N, []).
restrictLeft([], _, Diagonal) :- all_distinct(Diagonal).
restrictLeft([X|Xs], N, Acc) :-
        element(N, X, Elem),
        append(Acc, [Elem], App),
        restrictLeft(Xs, N, App).
restrictLeft([_|Xs], N, Acc) :-
        restrictLeft(Xs, N, Acc).

%imposes the restrictions regarding the sum of three vertices
innerSums([]).
innerSums([X|Xs]) :-
        innerSum(X),
        innerSums(Xs).
innerSum([T1, T2, T3, Sum]) :-
        T1 + T2 + T3 #= Sum.

%flattens the lines of vertices list into a list of vertices
flatten(List, Flat) :- flatten(List, Flat, []).
flatten([], Flat, Flat).
flatten([X|Xs], Flat, Acc) :-
		append(Acc, X, App),
		flatten(Xs, Flat, App).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trid generator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generateTrid(N, R, V, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trid display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printTrid(V, T).
