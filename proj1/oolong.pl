:- use_module(library(random)).
:- use_module(library(lists)).
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
:- dynamic currentPiece/1.

gameType('null').

start_1vs1 :- \+ gameType('1vs1'), \+ gameType('1vsAI'), \+ gameType('AIvsAI'), retract(gameType(null)),
				assert(gameType('1vs1')), nl, initGame, write('1 vs 1 game started successfully'), nl, printBoard.

start_1vsAI :- \+ gameType('1vs1'), \+ gameType('1vsAI'), \+ gameType('AIvsAI'), retract(gameType(null)),
				assert(gameType('1vsAI')), nl, initGame, write('1 vs AI game started successfully'), nl, printBoard.

start_AIvsAI :- \+ gameType('1vs1'), \+ gameType('1vsAI'), \+ gameType('AIvsAI'), retract(gameType(null)),
				assert(gameType('AIvsAI')), nl, initGame, write('AI vs AI game started successfully'), nl, printBoard.

initGame :- assert(currentPiece(b)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Movements and game logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkVictory :- checkVictoryPiece(b) ; checkVictoryPiece(g) ; fail.

checkVictoryPiece(Piece) :- fail.

flipCurrentPiece :- currentPiece(b) -> (retract(currentPiece(b)), assert(currentPiece(g))) ;
										(retract(currentPiece(g)), assert(currentPiece(b))).

getMove(X) :- gameType('AIvsAI'), !,getMoveAI(X).
getMove(X) :- gameType('1vsAI'), !, getMove1vsAI(X).
getMove(X) :- gameType('1vs1'), !, getMoveHuman(X).

getMove1vsAI(X) :- currentPiece(b) -> getMoveHuman(X) ; getMoveAI(X).

getMoveAI(X) :- random(0, 9, R), nth0(R, [n, s, e, w, nw, ne, sw, se, c], X).

getMoveHuman(X) :- nl, write('Position of piece '), currentPiece(Piece), write(Piece), write(' '),
					repeat,
						read(X),
						( (X == n ; X == s; X == e ; X== w ;
							X == ne; X == nw; X == se; X == sw;
							X == c ; X == stop) -> !, (X == stop -> break ; !) ;
								write('Invalid position, try again '), nl, fail).

move(X) :- 	currentPiece(Piece),		%gets current piece (black or green)
			waiterPos(T, P), 			%get current position of waiter
			retract(pos(T, X, _)),		%clear new position for piece
			assert(pos(T, X, Piece)),	%moves piece to new position
			retract(waiterPos(T, P)), 	%removes current waiter pos
			assert(waiterPos(X, T)),	%moves waiter to new pos
			flipCurrentPiece.			%changes the current piece

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Start
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(N):- N \= '1vs1', N \= '1vsAI', N \= 'AIvsAI', fail.
start('1vs1') :- start_1vs1.
start('1vsAI') :- start_1vsAI.
start('AIvsAI') :- start_AIvsAI.

startGame :-
		repeat,
			read(Type),
			(start(Type) -> ! ;
				write('Invalid option, try again '),
				nl,
				fail
			).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Game cycle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play :- nl, write('Choose the type of game you want to play (1vs1/1vsAI/AIvsAI)'), nl,
				startGame,							%initializes a match
				repeat,
						getMove(X),					%get move
						move(X),					%move piece
						printBoard,					%show board
						(checkVictory -> ! ; fail). %check victory (if fail, repeats)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
