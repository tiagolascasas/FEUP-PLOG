:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trid Entrypoint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Generates a Trid board of size N with NT triangle sums, solves it and prints the solution
trid(N, NT) :-
		generateTrid(N, NT, V, T),
		solveTrid(V, T),
		printTrid(V, T).

%Tests the solver with a predetermined board.
%Solution in http://rohanrao.blogspot.pt/2009/05/rules-of-trid.html
testSolver :-
        V = [[V1],[V2, V3],[V4, V5, V6],[V7, V8, V9, V10],[V11, V12, V13, V14, V15]],
        T = [[V2, V4, V5, 12],[V4, V7, V8, 9],[V7, V11, V12, 6],[V7, V11, V12, 6],[V8, V12, V13, 12],[V9, V13, V14, 8]],
        solveTrid(V, T),
        write(V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trid Solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Trid solver:
% V - list of lists with the vertices
% T - list of liss with the triangles and inner sums
solveTrid(V, T) :-
        length(V, Range),
		setDomain(V, Range),
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
        length(V, N),
        diagonalsRight(V, N).
diagonalsRight(_, 0).
diagonalsRight([V|Vx], N) :-
        restrictRight([V|Vx], N),
        M is N - 1,
        diagonalsRight(Vx, M).

restrictRight(V, N) :- restrictRight(V, N, [], 0).
restrictRight([], _, Diagonal, _) :- all_distinct(Diagonal).
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

%flattens the list of lists of vertices into a list of vertices
flatten(List, Flat) :- flatten(List, Flat, []).
flatten([], Flat, Flat).
flatten([X|Xs], Flat, Acc) :-
		append(Acc, X, App),
		flatten(Xs, Flat, App).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trid Generator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Generates a Trid board
% N - the number of rows
% NT - the number of inner triangle sums
% V - the generated board
% T - the generated inner sums
generateTrid(N, NT, V, T) :-
        N > 0,
        createBoard(N, V),
        createInnerValues(N, NT, T).

%creates a board of the specified dimension
createBoard(N, V) :- createBoard(N, V, [], 0).
createBoard(N, V, V, N).
createBoard(N, V, Acc, Index) :-
        I is Index + 1,
        length(Row, I),
        append(Acc, [Row], App),
        createBoard(N, V, App, I).

%creates inner triangle sums
createInnerValues(N, NT, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trid Displayer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printTrid(V, T) :- write(V).
