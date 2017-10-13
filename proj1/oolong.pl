:- use_module(library(random)).
:- use_module(library(system)).
:- include('positions.pl').

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
				write('0 - empty   B - Black   G - Green'), nl, nl.

printRow1 :- printTableTop(nw), write('   '), printTableTop(n), write('   '), printTableTop(ne).
printRow2 :- printTableMiddle(nw), write('   '), printTableMiddle(n), write('   '), printTableMiddle(ne).
printRow3 :- printTableBottom(nw), write('   '), printTableBottom(n), write('   '), printTableBottom(ne).

printRow4 :- printTableTop(nw), write('   '), printTableTop(n), write('   '), printTableTop(ne).
printRow5 :- printTableMiddle(nw), write('   '), printTableMiddle(n), write('   '), printTableMiddle(ne).
printRow6 :- printTableBottom(nw), write('   '), printTableBottom(n), write('   '), printTableBottom(ne).

printRow7 :- printTableTop(nw), write('   '), printTableTop(n), write('   '), printTableTop(ne).
printRow8 :- printTableMiddle(nw), write('   '), printTableMiddle(n), write('   '), printTableMiddle(ne).
printRow9 :- printTableBottom(nw), write('   '), printTableBottom(n), write('   '), printTableBottom(ne).

printTableTop(Table) :- printPos(Table, nw), write(' '), printPos(Table, n), write(' '), printPos(Table, ne).
printTableMiddle(Table) :- printPos(Table, w), write(' '), printPos(Table, c), write(' '), printPos(Table, e).
printTableBottom(Table) :- printPos(Table, sw), write(' '), printPos(Table, s), write(' '), printPos(Table, se).

printPos(Table, Pos) :- pos(Table, Pos, X), write(X).
