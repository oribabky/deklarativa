%%Lab 4 d7012e
%

different(A, B) :-
    not(A == B).

%facts
%
legalcolor(white).
legalcolor(black).

freecolor(green).

legalx(a)
%
%

%rules
%
%
availablelocation(Board, X, Y) :-
    legalmove(CheckColor, Board, X, Y),
    freecolor(CheckColor).

legalmove(Color, Board, X, Y) :-
    legalcolor(Color),
    availablelocation(Board, X, Y).


move( state( R, R, B, P, NR), Move, NewState) :-
    NR < 2,
    NewNR is NR+1,
    Move = pickupSteelKey,
    NewState = state( R, hasSteel, B, P, NewNR).












