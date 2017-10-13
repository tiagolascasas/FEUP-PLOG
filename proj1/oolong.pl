:- use_module(library(random)).
:- use_module(library(system)).
:- include('positions.pl').

%states of the game to use as an example
%:-include('estados/inicial.pl').
%:-include('estados/intermedio.pl').
%:-include('estados/final.pl').

printBoard :- 	nl,
				printRow1, nl,
				printRow2, nl,
				printRow3, nl, nl,
				printRow4, nl,
				printRow5, nl,
				printRow6, nl, nl,
				printRow7, nl,
				printRow8, nl,
				printRow9, nl, nl,
				write('0 - empty   b - Black   g - Green'), nl, nl.

printRow1 :- printTableTop(nw), write('   '), printTableTop(n), write('   '), printTableTop(ne).
printRow2 :- printTableMiddle(nw), write('   '), printTableMiddle(n), write('   '), printTableMiddle(ne).
printRow3 :- printTableBottom(nw), write('   '), printTableBottom(n), write('   '), printTableBottom(ne).

printRow4 :- printTableTop(w), write('   '), printTableTop(c), write('   '), printTableTop(e).
printRow5 :- printTableMiddle(w), write('   '), printTableMiddle(c), write('   '), printTableMiddle(e).
printRow6 :- printTableBottom(w), write('   '), printTableBottom(c), write('   '), printTableBottom(e).

printRow7 :- printTableTop(nw), write('   '), printTableTop(s), write('   '), printTableTop(se).
printRow8 :- printTableMiddle(sw), write('   '), printTableMiddle(s), write('   '), printTableMiddle(se).
printRow9 :- printTableBottom(sw), write('   '), printTableBottom(s), write('   '), printTableBottom(se).

printTableTop(Table) :- printPos(Table, nw), write(' '), printPos(Table, n), write(' '), printPos(Table, ne).
printTableMiddle(Table) :- printPos(Table, w), write(' '), printPos(Table, c), write(' '), printPos(Table, e).
printTableBottom(Table) :- printPos(Table, sw), write(' '), printPos(Table, s), write(' '), printPos(Table, se).

printPos(Table, Pos) :- pos(Table, Pos, X), write(X).
