:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trid Entrypoints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Generates a Trid board of size N with NT triangle sums, solves it and prints the solution
trid(N, NT) :-
        integer(N),
        integer(NT),
        nl, write('Generated inner sums:'), nl,
		generateTrid(N, NT, V, T),
		solveTrid(V, T),
		printTrid(V, T).

%Solves a trid board V with inner sums T and prints the solution
trid(V, T) :-
        solveTrid(V, T),
        printTrid(V, T).

%Tests the solver with a predetermined board.
%Solution in http://rohanrao.blogspot.pt/2009/05/rules-of-trid.html
testSolver :-
        V = [[V1],[V2, V3],[V4, V5, V6],[V7, V8, V9, V10],[V11, V12, V13, V14, V15]],
        T = [[V2, V4, V5, 12],[V4, V7, V8, 9],[V7, V11, V12, 6],[V7, V11, V12, 6],[V8, V12, V13, 12],[V9, V13, V14, 8]],
        trid(V, T).

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
        reset_timer,
		labeling([], Vf),
        print_time.

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

%predicates to calculate the labeling time
reset_timer :- statistics(walltime, _).
print_time :-
        statistics(walltime,[_,T]),
        TS is ((T//10)*10)/1000,
        nl, write('Time: '), write(TS), write('s'), nl, nl.

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
        createInnerSums(NT, V, T).

%creates a board of the specified dimension
createBoard(N, V) :- createBoard(N, V, [], 0).
createBoard(N, V, V, N).
createBoard(N, V, Acc, Index) :-
        I is Index + 1,
        length(Row, I),
        append(Acc, [Row], App),
        createBoard(N, V, App, I).

%creates inner triangle sums
createInnerSums(NT, V, T) :- createInnerSums(NT, V, T, [], 0).
createInnerSums(NT, _, T, T, NT).
createInnerSums(NT, V, T, Acc, Count) :-
        generateInnerSum(V, Acc, Sum),
        append(Acc, [Sum], App),
        CountInc is Count + 1,
        createInnerSums(NT, V, T, App, CountInc).

generateInnerSum(V, ExistingSums, Sum) :-
        %repeat,
        makeSum(V, Sum).
        %exists(Sum, ExistingSums),
        %fail.

makeSum(V, [TopVertex, LeftVertex, RightVertex, SumValue]) :-
        length(V, Size),
        Limit is Size - 1,
        random(1, Limit, TopRow),
        BottomRow is TopRow + 1,
        random(1, TopRow, TopVertexPos),
        LeftVertexPos is TopVertexPos,
        RightVertexPos is LeftVertexPos + 1,
        getVertex(V, TopRow, TopVertexPos, TopVertex),
        getVertex(V, BottomRow, LeftVertexPos, LeftVertex),
        getVertex(V, BottomRow, RightVertexPos, RightVertex),
        SumLimit is Size * 3,
        random(3, SumLimit, SumValue), nl,
        write('Vertex '), write(TopRow-TopVertexPos), write(' + '),
        write('vertex '), write(BottomRow-LeftVertexPos), write(' + '),
        write('vertex '), write(BottomRow-RightVertexPos), write(' = '),
        write(SumValue), nl.

getVertex(V, Row, Pos, Vertex) :- getVertex(V, Row, Pos, Vertex, 0).
getVertex([V|Vx], Row, Pos, Vertex, Row) :-
        nth1(Pos, V, Vertex).
getVertex([], _, _, _, _).
getVertex([_|Vx], Row, Pos, Vertex, Index) :-
        IndexInc is Index + 1,
        getVertex(Vx, Row, Pos, Vertex, IndexInc).

exists([V1, V2, V3, _], [V1, V2, V3, _ | _]).
exists(_, []) :- fail.
exists(Sum, [_|Vx]) :- exists(Sum, Vx).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trid Displayer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printTrid(V, T) :- length(V, R), R1 is R - 1,
                printTrid(V, T, R1).

printTrid([Line], _, R):-
                Amount is 3 * R, printSpaces(Amount),
                printLine(Line), nl.

printTrid([Line | Rest], T, R):-
                Amount is 3 * R, printSpaces(Amount),
                printLine(Line), nl, R1 is R - 1,
                IntrAmount is (R1 * 3) + 2,printSpaces(IntrAmount),
                printIntrLine(Line), nl,
                printTrid(Rest, T, R1).

printLine([Element]) :- numberLength(Element, ElementLength),
                Amount is 2 - ElementLength,
                printZeros(Amount), print(Element).
printLine([Element|Rest]) :- numberLength(Element, ElementLength),
                Amount is 2 - ElementLength,
                printZeros(Amount), print(Element),
                printHifen(4), printLine(Rest).

printIntrLine([_LineStart]):-write('/  \\').
printIntrLine([_LineStart|Rest]):- write('/  \\  '), printIntrLine(Rest).

%prints the given amount of zeros
printZeros(0).
printZeros(Amount):- write(0), A is Amount - 1, printSpaces(A).

%prints the given amount of spaces
printSpaces(0).
printSpaces(Amount):- write(' '), A is Amount - 1, printSpaces(A).

%prints the given amount of hifens
printHifen(0).
printHifen(Amount):- write('-'), A is Amount - 1, printHifen(A).

%counts the number of digits in a number
numberLength(0, 0).
numberLength(Number, Length):- X is Number//10, numberLength(X, L), Length is L + 1.
