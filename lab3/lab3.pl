%%Lab 3 d7012e
%

different(A, B) :-
    not(A == B).

move( state( R, S, B, hasPackage, NR), Move, NewState) :-
    NewNR is NR-1,
    Move = dropPackage,
    NewState = state( R, S, B, R, NewNR).

move( state( R, S, B, R, NR), Move, NewState) :-
    NR < 2,
    NewNR is NR+1,
    Move = pickupPackage,
    NewState = state( R, S, B, hasPackage, NewNR).

move( state( R, S, hasBrass, P, NR), Move, NewState) :-
    NewNR is NR-1,
    Move = dropBrassKey,
    NewState = state( R, S, R, P, NewNR).

move( state( R, S, R, P, NR), Move, NewState) :-
    NR < 2,
    NewNR is NR+1,
    Move = pickupBrassKey,
    NewState = state( R, S, hasBrass, P, NewNR).

move( state( R, hasSteel, B, P, NR), Move, NewState) :-
    NewNR is NR-1,
    Move = dropSteelKey,
    NewState = state( R, R, B, P, NewNR).

move( state( R, S, hasBrass, P, NR), Move, NewState) :-
    (R = r1; R = r3),
    (R2 = r1; R2 = r3),
    different(R, R2),
    Move = openBrassDoor( R, R2),
    NewState = state( R2, S, hasBrass, P, NR).

move( state( R, hasSteel, B, P, NR), Move, NewState) :-
    (R = r1; R = r2),
    (R2 = r1; R2 = r2),
    different(R, R2),
    Move = openSteelDoor( R, R2),
    NewState = state( R2, hasSteel, B, P, NR).

move( state( R, R, B, P, NR), Move, NewState) :-
    NR < 2,
    NewNR is NR+1,
    Move = pickupSteelKey,
    NewState = state( R, hasSteel, B, P, NewNR).

solveR( state(_, _, _, r2, _), _, []).

solveR( State1, N, [Move|Moves]) :-
    N > 0,
    NewN is N - 1,
    move( State1, Move, State2),
    solveR( State2, NewN, Moves).

%set_prolog_flag(answer_write_options,[max_depth(0)]).
%INITIAL STATE
temp(state( r1, r1, r2, r3, 0)).
start(Trace) :- temp(X), solveR(X, 14, Trace).




