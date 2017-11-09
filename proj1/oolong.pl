:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- include('positions.pl').

% Display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printBoard :- 	nl,
				printRow0, nl,
				printRow1, nl,
				printRow2, nl,
				printRow3, nl, nl,
				printRow4, nl,
				printRow5, nl,
				printRow6, nl, nl,
				printRow7, nl,
				printRow8, nl,
				printRow9, nl,
				printRow10, nl, nl,
				write('o - Empty g - Green  b - Black'), nl,
				printWaiterPos, nl, nl.

printRow0 :- write('                                 '), writeSpecMove(n).
printRow1 :- write('                                 '), printTableTop(nw), write('   '), printTableTop(n), write('   '), printTableTop(ne).
printRow2 :- writeSpecMove(nw), printTableMiddle(nw), write('   '), printTableMiddle(n), write('   '), printTableMiddle(ne), writeSpecMove(ne).
printRow3 :- write('                                 '), printTableBottom(nw), write('   '), printTableBottom(n), write('   '), printTableBottom(ne).

printRow4 :- write('                                 '), printTableTop(w), write('   '), printTableTop(c), write('   '), printTableTop(e).
printRow5 :- writeSpecMove(w), printTableMiddle(w), write('   '), printTableMiddle(c), write('   '), printTableMiddle(e), writeSpecMove(e).
printRow6 :- write('                                 '), printTableBottom(w), write('   '), printTableBottom(c), write('   '), printTableBottom(e).

printRow7 :- write('                                 '), printTableTop(sw), write('   '), printTableTop(s), write('   '), printTableTop(se).
printRow8 :- writeSpecMove(sw),                         printTableMiddle(sw), write('   '), printTableMiddle(s), write('   '), printTableMiddle(se), writeSpecMove(se).
printRow9 :- write('                                 '), printTableBottom(sw), write('   '), printTableBottom(s), write('   '), printTableBottom(se).
printRow10 :- write('                                '), writeSpecMove(s).

printTableTop(Table) :- printPos(Table, nw), write(' '), printPos(Table, n), write(' '), printPos(Table, ne).
printTableMiddle(Table) :- printPos(Table, w), write(' '), printPos(Table, c), write(' '), printPos(Table, e).
printTableBottom(Table) :- printPos(Table, sw), write(' '), printPos(Table, s), write(' '), printPos(Table, se).

printPos(Table, Pos) :- pos(Table, Pos, X), write(X).

printWaiterPos :- waiterPos(X, Y), write('Waiter in table '), write(X), write(' and position '), write(Y).

printCurrPlayerMove(Table, Pos) :- currentPiece(Player), write('Player '), write(Player), write(' placed a piece on table '),
									write(Table), write(', position '), write(Pos), nl.

printVictoryAnnouncement(Player) :- write('Player '), write(Player), write(' is victorious!'), nl.


writeSpecMove(Table) :- specMovePos(SpecMove, Table, AmountPieces, TargetP), writeSpecMove(SpecMove, AmountPieces, TargetP).
writeSpecMove(specialMovePiece, _, b) :-        write(' Move 1 black piece              ').
writeSpecMove(specialMovePiece, _, g) :-        write(' Move 1 green piece              ').
writeSpecMove(specialMoveWaiter, _, b) :-       write(' Black moves waiter              ').
writeSpecMove(specialMoveWaiter, _, g) :-       write(' Green moves waiter              ').
writeSpecMove(specialMoveRotate, _, _) :-       write(' Player rotates table            ').
writeSpecMove(specialMoveSwitch, 4, _) :-       write(' Switch 2 unconquered            ').
writeSpecMove(specialMoveSwitch, 5, _) :-       write(' Switch unconquered w/ conquered ').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Game settings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic gameType/1.
:- dynamic currentPiece/1.
:- dynamic specMovePos/4.
:- dynamic waiterSwitched/0.
:- dynamic newPosition/2.
:- dynamic specialMoveActive/1.
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
									(\+validPosition(Table, Position) -> ! ; fail).

getMoveAI(Table, Position) :- repeat,
								waiterPos(Table, _),
								random(0, 9, R), nth0(R, [n, s, e, w, nw, ne, sw, se, c], Position),
								(validPosition(Table, Position) -> ! ; fail).

%gets a move from the keyboard, testing for validity
getMoveHuman(Table, Position) :- waiterPos(T, _), checkFullTable(T), !,
									repeat,
										read(Table),
										((validInput(Table), \+checkFullTable(Table)) -> !, (Table == stop -> break ; !) ;
												write('Invalid table, try again '), nl, fail),
									repeat,
										read(Position),
										((validInput(Position), validPosition(Table, Position)) -> !, (Position == stop -> break ; !) ;
												write('Invalid position, try again '), nl, fail).


getMoveHuman(Table, Position) :- nl, write('Position of piece '), currentPiece(Piece), write(Piece), write(' '),
									waiterPos(Table, _),
									repeat,
										read(Position),
										((validInput(Position), validPosition(Table, Position)) -> !, (Position == stop -> break ; !) ;
												write('Invalid position, try again '), nl, fail).

%verifies if input is a valid table coord or the stop signal
validInput(X) :- X == n ; X == s; X == e ; X== w ;
					X == ne; X == nw; X == se; X == sw;
					X == c ; X == stop.

%verifies if the position within the waiter's table is valid
validPosition(Table, Position) :- pos(Table, Position, o), waiterPos(_, WP), Position \= WP.
validPosition(stop, _).
validPosition(_, stop).

%moves the current piece to the position X of the waiter's table
move(Table, Position) :-currentPiece(Piece),				%gets current piece (black or green)
						retract(pos(Table, Position, _)),	%clear new position for piece
						assert(pos(Table, Position, Piece)),%moves piece to new position
						assert(newPosition(Table, Position)),
						checkSpecialMove,%check if special moves apply (can use waiter pos to find table)
						(waiterSwitched -> retract(waiterSwitched);
						 retract(waiterPos(_, _)),                        %removes current waiter pos
						 assert(waiterPos(Position, Table))),	%moves waiter to new pos
						retract(newPosition(_, _)),
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
		 retractall(currentPiece(_)), retractall(specMovePos(_, _, _, _)), retractall(specialMoveActive(_)),
		 initPositions, initSpecMoves, assert(currentPiece(b)),
		 write('Game finished successfully'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Game cycle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%main game loop. It starts a new match and repeats an input-logic-display cycle until
%one of the players fulfills the victory conditions.
play :- nl, write('Choose the type of game you want to play (\'1vs1\'/\'1vsAI\'/\'AIvsAI\')'), nl,
				initPositions,
				initSpecMoves,
				startGame,										%initializes a match
				repeat,
						getMove(Table, Position),				%get move
						printCurrPlayerMove(Table, Position),	%prints the current player's move
						move(Table, Position),					%move piece
						printBoard,								%show board
						(checkVictory -> ! ; fail). 		%check victory (if fail, repeats)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% Special Moves
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Distribute special moves randomly in the tables
initSpecMoves :- initSpecMoves(8, [[specialMovePiece, 3, b], [specialMovePiece, 3, g], [specialMoveWaiter, 5, b], [specialMoveWaiter, 5, g],
				   [specialMoveRotate, 4, a], [specialMoveRotate, 4, a], [specialMoveSwitch, 4, a], [specialMoveSwitch, 5, a]],
				  [n, s, e, w, nw, ne, sw, se]),
				activateSpecMoves.	

initSpecMoves(_, [], []).
initSpecMoves(N, [[SpecMove, AmountPieces, TargetP| _]|Xs], L) :- random(0, N, R), nth0(R, L, Table),
		 assert(specMovePos(SpecMove, Table, AmountPieces, TargetP)),
		 delete(L, Table, L1),
		 N1 is N-1, initSpecMoves(N1, Xs, L1).

activateSpecMoves :- assert(specialMoveActive(nw)), assert(specialMoveActive(n)),
		     assert(specialMoveActive(ne)), assert(specialMoveActive(w)),
		     assert(specialMoveActive(e)), assert(specialMoveActive(sw)),
		     assert(specialMoveActive(s)), assert(specialMoveActive(se)).


%Check if any special move was activated with the last move
checkSpecialMove :- waiterPos(Table, _), Table = c, !.
checkSpecialMove :- waiterPos(Table, _),
		      specMovePos(SpecMove, Table, Amount, TargetP),
		      currentPiece(Player), getPieceCountOnTable(Player, Table, N),
		      ((N >= Amount, specialMoveActive(Table)) -> retract(specialMoveActive(Table)),
		              write('Special Move Unlocked:'), nl, writeSpecMove(Table), nl,
		              G =.. [SpecMove, Amount, TargetP], G; !).

%Special Move that allows one player to move one of its pieces from an unconquered table to another (one for each piece color) 
specialMovePiece(_, b) :- repeat, 
				write('Choose origin table '), read(OTable), nl, 
				write('Choose position '), read(OPosition), nl,
				((pos(OTable, OPosition, b) , getPieceCountOnTable(b, OTable, N1), N1 < 5, getPieceCountOnTable(g, OTable, N2), N2 < 5) 
				-> ! ; write('Invalid position, try again '), nl, fail),
                             repeat, 
                                write('Choose destiny table '), read(DTable), nl, 
                                write('Choose position '), read(DPosition), nl,
                                ((pos(DTable, DPosition, o) , getPieceCountOnTable(b, DTable,N3), N3 < 5, getPieceCountOnTable(g, DTable,N4), N4 < 5) 
				-> ! ; write('Invalid position, try again '), nl, fail),
			     specialMovePiece(OTable, OPosition, DTable, DPosition).
                             
				
specialMovePiece(_, g) :- repeat, 
                                write('Choose origin table '), read(OTable), nl, 
                                write('Choose position '), read(OPosition), nl,
                                ((pos(OTable, OPosition, g) , getPieceCountOnTable(b, OTable, N1), N1 < 5, getPieceCountOnTable(g, OTable, N2), N2 < 5) 
                                -> ! ; write('Invalid position, try again '), nl, fail),
                             repeat, 
                                write('Choose destiny table '), read(DTable), nl, 
                                write('Choose position '), read(DPosition), nl,
                                ((pos(DTable, DPosition, o) , getPieceCountOnTable(b, DTable,N3), N3 < 5, getPieceCountOnTable(g, DTable,N4), N4 < 5) 
                                -> ! ; write('Invalid position, try again '), nl, fail),
                             specialMovePiece(OTable, OPosition, DTable, DPosition).

specialMovePiece(T1, P1, T2, P2) :- pos(T1, P1, Player),
				retract(pos(T2, P2, _)),
				assert(pos(T2, P2, Player)),
				retract(pos(T1, P1, _)),
				assert(pos(T1, P1, o)).

%Special Move that allows one player to move the waiter to the same position on another table (one for each piece color) 
specialMoveWaiter(_, _) :- waiterPos(T, _), 
		             write('Choose destiny table '),
			     repeat,
				    read(Table),
				    ((Table \= T, pos(Table, _, _)) -> !; write('Invalid position, try again '), fail), 				
			     specialMoveWaiter(Table).

							
specialMoveWaiter(Table) :- newPosition(Position, _),
			     retract(waiterPos(_, _)),
			     assert(waiterPos(Table, Position)),
			     assert(waiterSwitched).

%Special Move that allows the last player that played to rotate the current table 
specialMoveRotate(_, _) :- write('Rotate current Table clockwise by '),
			      repeat,
				    read(Amount), nl,
				    (Amount >= 0,  Amount < 8 -> ! ; write('Invalid amount, try again '), fail),
	                      specialMoveRotate(Amount).

specialMoveRotate(Amount) :- waiterPos(Table, _), 
				tableToList(Table, [C | L1]),
				rotateListRight(L1, Amount, L2),
				append([C], L2, L3),
                                rebuildTable(Table, L3).
				
rotateListRight(L1, Amount, L2) :- append(L3, L4, L1),
				append(L4, L3, L2),
				length(L4, Amount).

%Special move that allows the last player that played to swap 2 unconquered tables
specialMoveSwitch(Amount, _) :- Amount = 4,
       				write('Choose first unconquered table: '), nl, 
				    repeat,
			                read(T1),
                                        (getPieceCountOnTable(b, T1, N1), N1 < 5, getPieceCountOnTable(g, T1, N2), N2 < 5 -> ! ; write('Invalid table, try again '), fail),
       			        write('Choose second unconquered table: '), nl,
				    repeat,
			                read(T2),
				        (getPieceCountOnTable(b, T2, N3), N3 < 5, getPieceCountOnTable(g, T2, N4), N4 < 5 -> ! ; write('Invalid table, try again '), fail),
                                specialMoveSwitch(T1, T2).

%Special move that allows the last player that played to swap an unconquered table with a conquered one
specialMoveSwitch(Amount, _) :- Amount = 5,
				write('Choose conquered table '), nl, 
                                    repeat,
				        read(T1),                                         
                                        (((getPieceCountOnTable(b, T1, N1), N1 >= 5) ; (getPieceCountOnTable(g, T1, N2), N2 >= 5)) -> ! ; write('Invalid table, try again: '), fail),
				write('Choose unconquered table '), nl,
                                    repeat,
                                     read(T2),
                                     ((getPieceCountOnTable(b, T2, N3), N3 < 5, getPieceCountOnTable(g, T2, N4), N4 < 5, T1 \= T2)-> ! ; write('Invalid table, try again: '), fail),
                                specialMoveSwitch(T1, T2).

specialMoveSwitch(T1, T2) :- tableToList(T1, L1), 
				tableToList(T2, L2), 
				rebuildTable(T1, L2), 
				rebuildTable(T2, L1), 
				waiterPos(Position, _),
				newPosition(_, Table),
				(Table == T1 -> assert(waiterSwitched),
                                        retract(waiterPos(_, _)),
					assert(waiterPos(T2, Position)) ; !,
				 (Table == T2 -> assert(waiterSwitched),
                                        retract(waiterPos(_, _)),
					assert(waiterPos(T1, Position)); !)).


%Auxiliary methods to turn table predicates into a list and vice-versa
rebuildTable(Table, [C, NW, N, NE, E, SE, S, SW, W| _]) :- retractall(pos(Table,_,_)),
                                assert(pos(Table, nw, NW)), assert(pos(Table, n, N)), assert(pos(Table, ne, NE)),
                                assert(pos(Table, w, W)), assert(pos(Table, c, C)), assert(pos(Table, e, E)),
                                assert(pos(Table, sw, SW)), assert(pos(Table, s, S)), assert(pos(Table, se, SE)).

tableToList(Table, L) :-pos(Table, nw, NW), pos(Table, n, N), pos(Table, ne, NE),
                        pos(Table, w, W), pos(Table, c, C), pos(Table, e, E),
                        pos(Table, sw, SW), pos(Table, s, S), pos(Table, se, SE),
			append([], [C, NW, N, NE, E, SE, S, SW, W] , L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

