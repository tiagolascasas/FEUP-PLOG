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

printCurrPlayerMove(Table, Pos) :- currentPiece(Player), write('Player '), write(Player), write(' placed a piece on table '),
									write(Table), write(', position '), write(Pos), nl.

printVictoryAnnouncement(Player) :- write('Player '), write(Player), write(' is victorious!'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Game settings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic gameType/1.
:- dynamic currentPiece/1.

%gameType('null').

start_1vs1 :- \+ gameType('1vs1'), \+ gameType('1vsAI'), \+ gameType('AIvsAI'), %retract(gameType(null)),
				assert(gameType('1vs1')), nl, initGame, write('1 vs 1 game started successfully'), nl, printBoard.

start_1vsAI :- \+ gameType('1vs1'), \+ gameType('1vsAI'), \+ gameType('AIvsAI'), %retract(gameType(null)),
				assert(gameType('1vsAI')), nl, initGame, write('1 vs AI game started successfully'), nl, printBoard.

start_AIvsAI :- \+ gameType('1vs1'), \+ gameType('1vsAI'), \+ gameType('AIvsAI'), %retract(gameType(null)),
				assert(gameType('AIvsAI')), nl, initGame, write('AI vs AI game started successfully'), nl, printBoard.

initGame :- assert(currentPiece(b)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Movements and game logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%checks if any of the players fulfill the victory conditions
checkVictory :- checkVictoryPlayer(b) ; checkVictoryPlayer(g) ; fail.

%checks if a player's pieces fulfill the victory conditions
checkVictoryPlayer(Player) :- countTablePieces(Player, [n, s, e, w, nw, ne, sw, se, c], N), N >= 5,
								printVictoryAnnouncement(Player), reset.

%counts the number of tables in which a player has 5 or more pieces
countTablePieces(_, [], 0).
countTablePieces(Player, [Table|Lis], M) :- getPieceCountOnTable(Player, Table, Cnt),
											Cnt >= 5, countTablePieces(Player, Lis, N), M is N + 1.
countTablePieces(Player, [_|Lis], N) :- countTablePieces(Player, Lis, N).

%gets the number of pieces of a certain player on a certain table
getPieceCountOnTable(Player, Table, N) :- findall(_, pos(Table, _, Player), Lis), length(Lis, N).

%checks if a given table is full
checkFullTable(Table) :- getPieceCountOnTable(o, Table, N), N == 0.

%changes the active player
flipCurrentPiece :- currentPiece(b) -> (retract(currentPiece(b)), assert(currentPiece(g))) ;
										(retract(currentPiece(g)), assert(currentPiece(b))).

%gets a move based on the game type
getMove(Table, Position) :- gameType('AIvsAI'), !,getMoveAI(Table, Position).
getMove(Table, Position) :- gameType('1vsAI'), !, getMove1vsAI(Table, Position).
getMove(Table, Position) :- gameType('1vs1'), !, getMoveHuman(Table, Position).

%gets a move under the 1 vs AI mode: if the current piece is black,
%gets it from the keyboard; otherwise, gets it from the AI
getMove1vsAI(Table, Position) :- currentPiece(b) -> getMoveHuman(Table, Position) ; getMoveAI(Table, Position).

%gets a move from the AI, testing for validity (randomizes a position until it is valid)
getMoveAI(Table, Position) :- waiterPos(T, _), checkFullTable(T), !,
								repeat,
									random(0, 9, R), nth0(R, [n, s, e, w, nw, ne, sw, se, c], Table),
									(\+checkFullTable(Table) -> ! ; fail),
								repeat,
									random(0, 9, R), nth0(R, [n, s, e, w, nw, ne, sw, se, c], Position),
									(\+validPosition(Position) -> ! ; fail).

getMoveAI(Table, Position) :- repeat,
								waiterPos(Table, _),
								random(0, 9, R), nth0(R, [n, s, e, w, nw, ne, sw, se, c], Position),
								(validPosition(Position) -> ! ; fail).

%gets a move from the keyboard, testing for validity
getMoveHuman(Table, Position) :- waiterPos(T, _), checkFullTable(T), !,
									repeat,
										read(Table),
										((validInput(Table), \+checkFullTable(Table)) -> !, (Table == stop -> break ; !) ;
												write('Invalid table, try again '), nl, fail),
									repeat,
										read(Position),
										((validInput(Position), validPosition(Position)) -> !, (Position == stop -> break ; !) ;
												write('Invalid position, try again '), nl, fail).


getMoveHuman(Table, Position) :- nl, write('Position of piece '), currentPiece(Piece), write(Piece), write(' '),
									waiterPos(Table, _),
									repeat,
										read(Position),
										((validInput(Position), validPosition(Position)) -> !, (Position == stop -> break ; !) ;
												write('Invalid position, try again '), nl, fail).

%verifies if input is a valid table coord or the stop signal
validInput(X) :- X == n ; X == s; X == e ; X== w ;
					X == ne; X == nw; X == se; X == sw;
					X == c ; X == stop.

%verifies if the position within the waiter's table is valid
validPosition(Table, Position) :- pos(Table, Position, o).
validPosition(stop, _).
validPosition(_, stop).

%moves the current piece to the position X of the waiter's table
move(Table, Position) :-currentPiece(Piece),				%gets current piece (black or green)
						retract(pos(Table, Position, _)),	%clear new position for piece
						assert(pos(Table, Position, Piece)),%moves piece to new position
						retract(waiterPos(_, _)), 			%removes current waiter pos
						assert(waiterPos(Position, Table)),	%moves waiter to new pos
						flipCurrentPiece.					%changes the current piece

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Start and end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%starts a new game based on a given game type
start(N):- N \= '1vs1', N \= '1vsAI', N \= 'AIvsAI', fail.
start('1vs1') :- start_1vs1.
start('1vsAI') :- start_1vsAI.
start('AIvsAI') :- start_AIvsAI.

%starts a new game, reading from the keyboard a valid game type
startGame :-
		repeat,
			read(Type),
			(start(Type) -> ! ;
				write('Invalid option, try again '),
				nl,
				fail
			).

reset :- retractall(pos(_,_,_)), retractall(gameType(_)), retractall(waiterPos(_,_)),
		 retractall(currentPiece(_)), initPositions, assert(currentPiece(b)),
		 write('Game finished successfully'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Game cycle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%main game loop. It starts a new match and repeats an input-logic-display cycle until
%one of the players fulfills the victory conditions.
play :- nl, write('Choose the type of game you want to play (1vs1/1vsAI/AIvsAI)'), nl,
				initPositions,
				startGame,										%initializes a match
				repeat,
						getMove(Table, Position),				%get move
						printCurrPlayerMove(Table, Position),	%prints the current player's move
						move(Table, Position),					%move piece
						printBoard,								%show board
						(checkVictory -> ! ; fail). 		%check victory (if fail, repeats)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
