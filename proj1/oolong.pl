:- use_module(library(random)).
:- use_module(library(system)).
:- include('positions.pl').

% Display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
				write('o - Empty g - Green  b - Black'), nl,
				printWaiterPos, nl, nl.

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

printWaiterPos :- waiterPos(X, Y), write('Waiter in table '), write(X), write(' and position '), write(Y).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Game settings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic gameType/1.
:- dynamic gameRunning/1.
gameType('null').
gameRunning('null').

start_1vs1 :- \+ gameType('1vs1'), \+ gameType('1vsAI'), \+ gameType('AIvsAI'), 
				assert(gameType('1vs1')), nl, initGame, write('1 vs 1 game started successfully'), nl, printBoard.

start_1vsAI :- \+ gameType('1vs1'), \+ gameType('1vsAI'), \+ gameType('AIvsAI'), 
				assert(gameType('1vsAI')), nl, initGame, write('1 vs AI game started successfully'), nl, printBoard.

start_AIvsAI :- \+ gameType('1vs1'), \+ gameType('1vsAI'), \+ gameType('AIvsAI'), 
				assert(gameType('AIvsAI')), nl, initGame, write('AI vs AI game started successfully'), nl, printBoard.
				
initGame :- assert(gameRunning('yes')).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Movements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Start
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start('1vs1') :- start_1vs1.
start('1vsAI') :- start_1vsAI.
start('AIvsAI') :- start_AIvsAI.

start(N):- N \= '1vs1', N \= '1vsAI', N \= 'AIvsAI', fail.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Game cycle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play :- nl, write('Choose the type of game you want to play (1vs1/1vsAI/AIvsAI)'), nl,
				repeat,	
				    read(Type), 
				    (\+ start(Type) -> 
				            write('invalid option, try again:'),
					    nl,
					    fail
				    ),
				repeat
	                            %get move
	                            %move piece
	                            %show board
	                            %check victory (if fail, repeats)
	                            .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
