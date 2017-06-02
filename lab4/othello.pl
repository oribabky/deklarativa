/* ----------------------------------------------------------
    CSE 3401 F12 Assignment 4 file

% Surname:
% First Name:
% Student Number:

  ------------------------------------------------------ */

%do not chagne the follwoing line!
:- ensure_loaded('play.pl').
:- ensure_loaded('testboards.pl').
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers (feel free to add your own helpers if needed,
%       MAKE SURE to write comments for all your helpers, marks will
%       be deducted for bad style!).
%
%       Implement the following predicates at their designated space
%       in this file (we suggest to have a look at file ttt.pl  to
%       see how the implementations is done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr)
%          * tie(State)
%          * terminal(State)
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */





% /* ------------------------------------------------------ */

% We use the following State Representation:
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows:
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position.

player(1).
player(2).

different(A, B) :-
    not(A == B).

oppositePlayer(Plyr, Opp) :-
	player(Plyr),
	player(Opp),
	different(Plyr, Opp).

% given helper: Inital state of the board
initBoard([ [.,.,.,.,.,.],
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.],
	    [.,.,2,1,.,.],
            [.,.,.,.,.,.],
	    [.,.,.,.,.,.] ]).

%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr).
%%%  holds iff InitialState is the initial state and
%%%  InitialPlyr is the player who moves first.

initialize(InitialState, 1):-
	initBoard(InitialState).

%try another board
%initialize(InitialState, 1):-
%	testBoard11(InitialState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player

winner(State, Plyr) :-
	oppositePlayer(Plyr, OtherPlyr),
	noAvailableMoves(State, OtherPlyr),
        noAvailableMoves(State, Plyr),
	countSumWeightsPlayer(State, Plyr, PlyrSum),
	countSumWeightsPlayer(State, OtherPlyr, OtherPlyrSum),
	PlyrSum < OtherPlyrSum.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Count weights/tiles%%%%%%%%%%%%%%%%%%%

countSumWeightsRow( [], _, 0).

countSumWeightsRow( [Plyr|Weights], Plyr, Sum) :-
	countSumWeightsRow( Weights, Plyr, NewSum),
	Sum is NewSum + 1.

countSumWeightsRow( [NotPlyr|Weights], Plyr, Sum) :-
	NotPlyr \= Plyr,
	countSumWeightsRow( Weights, Plyr, Sum).

countSumWeightsPlayer( [], _, 0).

countSumWeightsPlayer( [Row|Rows], Plyr, Sum) :-
	countSumWeightsRow( Row, Plyr, RowSum),
	countSumWeightsPlayer( Rows, Plyr, RestSum),
	Sum is RestSum + RowSum.

%%%%%%%%%%%%%%%%%%%%%%%%%% Count weights/tiles%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here.
%    - true if terminal State is a "tie" (no winner)
tie(State) :-
	player(Plyr),
        oppositePlayer(Plyr, OtherPlyr),
        noAvailableMoves(State, Plyr),
        noAvailableMoves(State, OtherPlyr),
	countSumWeightsPlayer(State, Plyr, PlyrSum),
	countSumWeightsPlayer(State, OtherPlyr, OtherPlyrSum),
	PlyrSum = OtherPlyrSum.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State).
%   - true if State is a terminal

terminal(State) :- winner(State,_).
terminal(State) :- tie(State).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%
showState( G ) :-
	printRows( G ).

printRows( [] ).
printRows( [H|L] ) :-
	printList(H),
	nl,
	printRows(L).

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).


%%%%%translation of positions (1-36) to board coordinates.

position(1, X) :- X = [0,0].
position(2, X) :- X = [1,0].
position(3, X) :- X = [2,0].
position(4, X) :- X = [3,0].
position(5, X) :- X = [4,0].
position(6, X) :- X = [5,0].
position(7, X) :- X = [0,1].
position(8, X) :- X = [1,1].
position(9, X) :- X = [2,1].
position(10, X) :- X = [3,1].
position(11, X) :- X = [4,1].
position(12, X) :- X = [5,1].
position(13, X) :- X = [0,2].
position(14, X) :- X = [1,2].
position(15, X) :- X = [2,2].
position(16, X) :- X = [3,2].
position(17, X) :- X = [4,2].
position(18, X) :- X = [5,2].
position(19, X) :- X = [0,3].
position(20, X) :- X = [1,3].
position(21, X) :- X = [2,3].
position(22, X) :- X = [3,3].
position(23, X) :- X = [4,3].
position(24, X) :- X = [5,3].
position(25, X) :- X = [0,4].
position(26, X) :- X = [1,4].
position(27, X) :- X = [2,4].
position(28, X) :- X = [3,4].
position(29, X) :- X = [4,4].
position(30, X) :- X = [5,4].
position(31, X) :- X = [0,5].
position(32, X) :- X = [1,5].
position(33, X) :- X = [2,5].
position(34, X) :- X = [3,5].
position(35, X) :- X = [4,5].
position(36, X) :- X = [5,5].

position([A,B], X) :-
    X = [A,B],
    A >= 0,
    A =< 5,
    B >= 0,
    B =< 5.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define moves(Plyr,State,MvList).
%   - returns list MvList of all legal moves Plyr can make in State
%


noAvailableMoves(State, Plyr) :-
	moves(Plyr, State, MvList),
        member(n, MvList).

moves(Plyr,State,MvList) :-
  testmoves(Plyr, 36,State,[],MvList),

% if there are no available moves:
  length(MvList, Len),
  Len > 0, !
  ;
  append([], ['n'], MvList).

%%%%%test all possible moves
testmoves(Plyr, Pos,St,SoFar,MvList) :-
  Pos > 0,
  validmove(Plyr,St,Pos), !,
  NextPos is Pos - 1,
  testmoves(Plyr, NextPos,St,[Pos|SoFar],MvList).

testmoves(Plyr, Pos,St,SoFar,MvList) :-
  Pos > 0,
  NextPos is Pos - 1,
  testmoves(Plyr, NextPos,St,SoFar,MvList).

testmoves(_,0,_,MvList,MvList).



%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%%
%% define nextState(Plyr,Move,State,NewState,NextPlyr).
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next
%     state) and NextPlayer (i.e. the next player who will move).
%
nextState(Plyr, 'n', State, State, NextPlyr) :-
    oppositePlayer( Plyr, NextPlyr), !.
nextState(Plyr, Move, State, State9, NextPlyr) :-
	position(Move, Coords),
        set( State, State1, Coords, Plyr),
        oppositePlayer( Plyr, NextPlyr),
	flipTilesRight(State1, Plyr, Coords, State2),
	flipTilesUpRight(State2, Plyr, Coords, State3),
	flipTilesUp(State3, Plyr, Coords, State4),
	flipTilesUpLeft(State4, Plyr, Coords, State5),
	flipTilesLeft(State5, Plyr, Coords, State6),
	flipTilesDownLeft(State6, Plyr, Coords, State7),
	flipTilesDown(State7, Plyr, Coords, State8),
	flipTilesDownRight(State8, Plyr, Coords, State9).


% these functions will flip all opposite tiles in a given direction
% if the next tile belongs to the opposite player and a tile of the same
% player is found further along the direction.
%

% fliptiles always succeeds. It will return the original state if tiles
% cannot be flipped along a path.
flipTilesRight(State, Plyr, Coords, FinalState) :-
	nextRight(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipRight(State, Plyr, NextCoords, NextValue, State, FinalState).
flipTilesRight(State, _, _, FinalState) :-
    append([], State, FinalState).

flipRight(State, Plyr, _, Plyr, _, FinalState) :-
    append([], State, FinalState).
flipRight(State, Plyr, Coords, Value, OriginalState, FinalState) :-
	player(Value),
	different(Plyr, Value),
	set( State, NewState, Coords, Plyr),
	nextRight(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipRight(NewState, Plyr, NextCoords, NextValue, OriginalState, FinalState).
flipRight(_, _, _, _, OriginalState, FinalState) :-
    append([], OriginalState, FinalState).


flipTilesUpRight(State, Plyr, Coords, FinalState) :-
	nextUpRight(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipUpRight(State, Plyr, NextCoords, NextValue, State, FinalState).
flipTilesUpRight(State, _, _, FinalState) :-
    append([], State, FinalState).


flipUpRight(State, Plyr, _, Plyr, _, FinalState) :-
    append([], State, FinalState).
flipUpRight(State, Plyr, Coords, Value, OriginalState, FinalState) :-
	player(Value),
	different(Plyr, Value),
	set( State, NewState, Coords, Plyr),
	nextUpRight(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipUpRight(NewState, Plyr, NextCoords, NextValue, OriginalState, FinalState).
flipUpRight(_, _, _, _, OriginalState, FinalState) :-
    append([], OriginalState, FinalState).



flipTilesUp(State, Plyr, Coords, FinalState) :-
	nextUp(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipUp(State, Plyr, NextCoords, NextValue, State, FinalState).
flipTilesUp(State, _, _, FinalState) :-
    append([], State, FinalState).

flipUp(State, Plyr, _, Plyr, _, FinalState) :-
    append([], State, FinalState).
flipUp(State, Plyr, Coords, Value, OriginalState, FinalState) :-
	player(Value),
	different(Plyr, Value),
	set( State, NewState, Coords, Plyr),
	nextUp(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipUp(NewState, Plyr, NextCoords, NextValue, OriginalState, FinalState).
flipUp(_, _, _, _, OriginalState, FinalState) :-
    append([], OriginalState, FinalState).



flipTilesUpLeft(State, Plyr, Coords, FinalState) :-
	nextUpLeft(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipUpLeft(State, Plyr, NextCoords, NextValue, State, FinalState).
flipTilesUpLeft(State, _, _, FinalState) :-
    append([], State, FinalState).


flipUpLeft(State, Plyr, _, Plyr, _, FinalState) :-
    append([], State, FinalState).
flipUpLeft(State, Plyr, Coords, Value, OriginalState, FinalState) :-
	player(Value),
	different(Plyr, Value),
	set( State, NewState, Coords, Plyr),
	nextUpLeft(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipUpLeft(NewState, Plyr, NextCoords, NextValue, OriginalState, FinalState).
flipUpLeft(_, _, _, _, OriginalState, FinalState) :-
    append([], OriginalState, FinalState).



flipTilesLeft(State, Plyr, Coords, FinalState) :-
	nextLeft(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipLeft(State, Plyr, NextCoords, NextValue, State, FinalState).
flipTilesLeft(State, _, _, FinalState) :-
    append([], State, FinalState).


flipLeft(State, Plyr, _, Plyr, _, FinalState) :-
    append([], State, FinalState).
flipLeft(State, Plyr, Coords, Value, OriginalState, FinalState) :-
	player(Value),
	different(Plyr, Value),
	set( State, NewState, Coords, Plyr),
	nextLeft(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipLeft(NewState, Plyr, NextCoords, NextValue, OriginalState, FinalState).
flipLeft(_, _, _, _, OriginalState, FinalState) :-
    append([], OriginalState, FinalState).



flipTilesDownLeft(State, Plyr, Coords, FinalState) :-
	nextDownLeft(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipDownLeft(State, Plyr, NextCoords, NextValue, State, FinalState).
flipTilesDownLeft(State, _, _, FinalState) :-
    append([], State, FinalState).


flipDownLeft(State, Plyr, _, Plyr, _, FinalState) :-
    append([], State, FinalState).
flipDownLeft(State, Plyr, Coords, Value, OriginalState, FinalState) :-
	player(Value),
	different(Plyr, Value),
	set( State, NewState, Coords, Plyr),
	nextDownLeft(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipDownLeft(NewState, Plyr, NextCoords, NextValue, OriginalState, FinalState).
flipDownLeft(_, _, _, _, OriginalState, FinalState) :-
    append([], OriginalState, FinalState).



flipTilesDown(State, Plyr, Coords, FinalState) :-
	nextDown(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipDown(State, Plyr, NextCoords, NextValue, State, FinalState).
flipTilesDown(State, _, _, FinalState) :-
    append([], State, FinalState).


flipDown(State, Plyr, _, Plyr, _, FinalState) :-
    append([], State, FinalState).
flipDown(State, Plyr, Coords, Value, OriginalState, FinalState) :-
	player(Value),
	different(Plyr, Value),
	set( State, NewState, Coords, Plyr),
	nextDown(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipDown(NewState, Plyr, NextCoords, NextValue, OriginalState, FinalState).
flipDown(_, _, _, _, OriginalState, FinalState) :-
    append([], OriginalState, FinalState).



flipTilesDownRight(State, Plyr, Coords, FinalState) :-
	nextDownRight(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipDownRight(State, Plyr, NextCoords, NextValue, State, FinalState).
flipTilesDownRight(State, _, _, FinalState) :-
    append([], State, FinalState).

flipDownRight(State, Plyr, _, Plyr, _, FinalState) :-
    append([], State, FinalState).
flipDownRight(State, Plyr, Coords, Value, OriginalState, FinalState) :-
	player(Value),
	different(Plyr, Value),
	set( State, NewState, Coords, Plyr),
	nextDownRight(Coords, NextCoords),
	get( State, NextCoords, NextValue),
	flipDownRight(NewState, Plyr, NextCoords, NextValue, OriginalState, FinalState).
flipDownRight(_, _, _, _, OriginalState, FinalState) :-
    append([], OriginalState, FinalState).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%%
%% define validmove(Plyr,State,Proposed).
%   - true if Proposed move by Plyr is valid at State.

%
%These relations give the next coordinate in a given direction.
%
nextRight([X1,Y], [X2,Y]) :-
	X2 is X1 + 1,
	X2 =< 5.
nextUpRight([X1, Y1], [X2, Y2]) :-
	X2 is X1 + 1,
	X2 =< 5,
	Y2 is Y1 - 1,
	Y2 >= 0.
nextUp([X, Y1], [X, Y2]) :-
	Y2 is Y1 - 1,
	Y2 >= 0.
nextUpLeft([X1, Y1], [X2, Y2]) :-
	X2 is X1 - 1,
	X2 >= 0,
	Y2 is Y1 - 1,
	Y2 >= 0.
nextLeft([X1, Y], [X2, Y]) :-
	X2 is X1 - 1,
	X2 >= 0.
nextDownLeft([X1, Y1], [X2, Y2]) :-
	X2 is X1 - 1,
	X2 >= 0,
	Y2 is Y1 + 1,
	Y2 =< 5.
nextDown([X, Y1], [X, Y2]) :-
	Y2 is Y1 + 1,
	Y2 =< 5.
nextDownRight([X1, Y1], [X2, Y2]) :-
	X2 is X1 + 1,
	X2 =< 5,
	Y2 is Y1 + 1,
	Y2 =< 5.

%
%These recursive functions finds a player in a given direction
%
findPlyrRight(_, Plyr, _, Plyr).
findPlyrRight(State, Plyr, Cord, Value) :-
	player(Value),
	different(Plyr, Value),
	nextRight(Cord, NextCord),
	get( State, NextCord, NextValue),
	findPlyrRight( State, Plyr, NextCord, NextValue).

findPlyrUpRight(_, Plyr, _, Plyr).
findPlyrUpRight(State, Plyr, Cord, Value) :-
	player(Value),
	different(Plyr, Value),
	nextUpRight(Cord, NextCord),
	get( State, NextCord, NextValue),
	findPlyrUpRight( State, Plyr, NextCord, NextValue).

findPlyrUp(_, Plyr, _, Plyr).
findPlyrUp(State, Plyr, Cord, Value) :-
	player(Value),
	different(Plyr, Value),
	nextUp(Cord, NextCord),
	get( State, NextCord, NextValue),
	findPlyrUp( State, Plyr, NextCord, NextValue).

findPlyrUpLeft(_, Plyr, _, Plyr).
findPlyrUpLeft(State, Plyr, Cord, Value) :-
	player(Value),
	different(Plyr, Value),
	nextUpLeft(Cord, NextCord),
	get( State, NextCord, NextValue),
	findPlyrUpLeft( State, Plyr, NextCord, NextValue).

findPlyrLeft(_, Plyr, _, Plyr).
findPlyrLeft(State, Plyr, Cord, Value) :-
	player(Value),
	different(Plyr, Value),
	nextLeft(Cord, NextCord),
	get( State, NextCord, NextValue),
	findPlyrLeft( State, Plyr, NextCord, NextValue).

findPlyrDownLeft(_, Plyr, _, Plyr).
findPlyrDownLeft(State, Plyr, Cord, Value) :-
	player(Value),
	different(Plyr, Value),
	nextDownLeft(Cord, NextCord),
	get( State, NextCord, NextValue),
	findPlyrDownLeft( State, Plyr, NextCord, NextValue).

findPlyrDown(_, Plyr, _, Plyr).
findPlyrDown(State, Plyr, Cord, Value) :-
	player(Value),
	different(Plyr, Value),
	nextDown(Cord, NextCord),
	get( State, NextCord, NextValue),
	findPlyrDown( State, Plyr, NextCord, NextValue).

findPlyrDownRight(_, Plyr, _, Plyr).
findPlyrDownRight(State, Plyr, Cord, Value) :-
	player(Value),
	different(Plyr, Value),
	nextDownRight(Cord, NextCord),
	get( State, NextCord, NextValue),
	findPlyrDownRight( State, Plyr, NextCord, NextValue).

%
%These functions checks that a tile placement is ok in a given direction
%

okRight(Plyr, OppPlyr, State, Coords) :-
	nextRight(Coords, NextCord), %check that the next tile is
	get( State, NextCord, NextValue),  %the other player.
	NextValue = OppPlyr,

	%starting from NextCord, find another tile that belongs to Plyr
	findPlyrRight(State, Plyr, NextCord, NextValue).

okUpRight(Plyr, OppPlyr, State, Coords) :-
	nextUpRight(Coords, NextCord), %check that the next tile is
	get( State, NextCord, NextValue),  %the other player.
	NextValue = OppPlyr,

	%starting from NextCord, find another tile that belongs to Plyr
	findPlyrUpRight(State, Plyr, NextCord, NextValue).

okUp(Plyr, OppPlyr, State, Coords) :-
	nextUp(Coords, NextCord), %check that the next tile is
	get( State, NextCord, NextValue),  %the other player.
	NextValue = OppPlyr,

	%starting from NextCord, find another tile that belongs to Plyr
	findPlyrUp(State, Plyr, NextCord, NextValue).

okUpLeft(Plyr, OppPlyr, State, Coords) :-
	nextUpLeft(Coords, NextCord), %check that the next tile is
	get( State, NextCord, NextValue),  %the other player.
	NextValue = OppPlyr,

	%starting from NextCord, find another tile that belongs to Plyr
	findPlyrUpLeft(State, Plyr, NextCord, NextValue).

okLeft(Plyr, OppPlyr, State, Coords) :-
	nextLeft(Coords, NextCord), %check that the next tile is
	get( State, NextCord, NextValue),  %the other player.
	NextValue = OppPlyr,

	%starting from NextCord, find another tile that belongs to Plyr
	findPlyrLeft(State, Plyr, NextCord, NextValue).

okDownLeft(Plyr, OppPlyr, State, Coords) :-
	nextDownLeft(Coords, NextCord), %check that the next tile is
	get( State, NextCord, NextValue),  %the other player.
	NextValue = OppPlyr,

	%starting from NextCord, find another tile that belongs to Plyr
	findPlyrDownLeft(State, Plyr, NextCord, NextValue).

okDown(Plyr, OppPlyr, State, Coords) :-
	nextDown(Coords, NextCord), %check that the next tile is
	get( State, NextCord, NextValue),  %the other player.
	NextValue = OppPlyr,

	%starting from NextCord, find another tile that belongs to Plyr
	findPlyrDown(State, Plyr, NextCord, NextValue).

okDownRight(Plyr, OppPlyr, State, Coords) :-
	nextDownRight(Coords, NextCord), %check that the next tile is
	get( State, NextCord, NextValue),  %the other player.
	NextValue = OppPlyr,

	%starting from NextCord, find another tile that belongs to Plyr
	findPlyrDownRight(State, Plyr, NextCord, NextValue).


%checks for a valid move, given a certain pos.
validmove(Plyr, State, 'n') :-
    noAvailableMoves(State, Plyr).

validmove(Plyr, State, ProposedPos) :-
	%check that the position is empty
	position(ProposedPos, Coords),
	get( State, Coords, Value),
	Value = '.',
	%check that there is a tile of the opposite player between
	%the proposed position, and another one of Plyr. Check in all
	%directions.
	oppositePlayer(Plyr, OppPlyr),
	(okRight(Plyr, OppPlyr, State, Coords);
	okUpRight(Plyr, OppPlyr, State, Coords);
	okUp(Plyr, OppPlyr, State, Coords);
	okUpLeft(Plyr, OppPlyr, State, Coords);
	okLeft(Plyr, OppPlyr, State, Coords);
	okDownLeft(Plyr, OppPlyr, State, Coords);
	okDown(Plyr, OppPlyr, State, Coords);
	okDownRight(Plyr, OppPlyr, State, Coords)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define h(State,Val).
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

%if ( Max Player Moves + Min Player Moves != 0)
%	Mobility Heuristic Value =
%               100 * (Max Player Moves - Min Player Moves) / (Max
%               Player Moves + Min Player Moves)
%else
%	Mobility Heuristic Value = 0

h(State,101) :- winner(State,1), !. %MAX
h(State,-101) :- winner(State,2), !. %MIN
h(State,0) :- tie(State), !.

% AI tactic is to ensure that the other player has as many moves
% available as possible.
h(State, Val) :-
    moves(1,State,MvListPlyr1),
    moves(2, State, MvListPlyr2),
   % length(MvListPlyr1, NrMovesPlyr1),
   % length(MvListPlyr2, NrMovesPlyr2),
   nrMoves(MvListPlyr1, NrMovesPlyr1),
   nrMoves(MvListPlyr2, NrMovesPlyr2),
    different(NrMovesPlyr1, NrMovesPlyr2),

    %tactic of AI to ensure that the other player has as many moves as possible.
    Val is (100 * (NrMovesPlyr2 - NrMovesPlyr1) / (NrMovesPlyr1 + NrMovesPlyr2)),

    %the other tactic to ensure the other player has as few moves as possible:
    %Val is (100 * (NrMovesPlyr1 - NrMovesPlyr2) / (NrMovesPlyr1 + NrMovesPlyr2)),
    !.

h(_, 0).

nrMoves(MvList, Nr) :-
    member('n', MvList),
    Nr is 0.

nrMoves(MvList, Nr) :-
    length(MvList, Nr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define lowerBound(B).
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-102).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define upperBound(B).
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(102).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value).
%. . . . . .
%. . . . . .
%. . 1 2 . .
%. . 2 1 . .
%. . . . . .
%. . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'],
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...],
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2
%Yes
%?-
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1), set(NB1, NB2, [2,3], 1),  showState(NB2).
%
% . . . . . .
% . . . . . .
% . . 1 2 . .
% . . 2 1 . .
% . . . . . .
% . . . . . .
%
% . . . . . .
% . . . . . .
% . . 1 2 . .
% . . 1 1 . .
% . . 1 . . .
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.',
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.',
%'.', '.'|...]]

% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
get( Board, [X, Y], Value) :-
	nth0( Y, Board, ListY),
	nth0( X, ListY, Value).

% set( Board, NewBoard, [X, Y], Value)

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value)
    :- setInList(Row, NewRow, X, Value).

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
	Y > 0,
	Y1 is Y-1,
	set( RestRows, NewRestRows, [X, Y1], Value).

% setInList( List, NewList, Index, Value)

setInList( [_|RestList], [Value|RestList], 0, Value).

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :-
	Index > 0,
	Index1 is Index-1,
	setInList( RestList, NewRestList, Index1, Value).

