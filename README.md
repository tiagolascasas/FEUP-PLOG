# PLOG1718

Relatório T1 intercalar: https://www.overleaf.com/11573667wmgksdpkmvng#/43774117/

Relatório T1 final: https://www.overleaf.com/12108699mxttrhvwvwwt#/45985626/

Relatório T2 final:

printInnerSums(_, []).
printInnerSums(V, [T|Ts]) :-
        printInnerSum(V, T),
        printInnerSums(V, Ts).

printInnerSum(V, [V1, V2, V3, Sum]) :-
        getVertexPos(V, V1, Pos1),
        getVertexPos(V, V2, Pos2),
        getVertexPos(V, V3, Pos3),
        write('Vertex '),  write(Pos1), write(' + '),
        write('Vertex '),  write(Pos2), write(' + '),
        write('Vertex '),  write(Pos3), write(' = '),
        write(Sum), nl, nl.

getVertexPos([], _, _).
getVertexPos(V, Vertex, Pos) :- getVertexPos(V, Vertex, Pos, 1).
getVertexPos([V|_], Vertex, Count-Index, Count) :-
        member(Vertex, V),
        element(Index, V, Vertex).
getVertexPos([_|Vx], Vertex, Pos, Count) :-
        CountInc is Count + 1,
        getVertexPos(Vx, Vertex, Pos, CountInc).

vertexInRow(_, []) :- fail.
vertexInRow(Vertex, [V|_]) :-
        Vertex == V.
vertexInRow(Vertex, [_|Vx]) :-
        vertexInRow(Vertex, Vx).
