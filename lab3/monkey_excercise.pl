%% Demo coming from http://clwww.essex.ac.uk/course/LG519/2-facts/index_18.html
%%
%% Please load this file into SWI-Prolog
%%
%% Sam's likes and dislikes in food
%%
%% Considering the following will give some practice
%% in thinking about backtracking.
%%
%% You can also run this demo online at
%% http://swish.swi-prolog.org/?code=https://github.com/SWI-Prolog/swipl-devel/raw/master/demo/likes.pl&q=likes(sam,Food).

/** <examples>
?- likes(sam,dahl).
?- likes(sam,chop_suey).
?- likes(sam,pizza).
?- likes(sam,chips).
?- likes(sam,curry).
*/

likes(sam,Food) :-
    indian(Food),
    mild(Food).
likes(sam,Food) :-
    chinese(Food).
likes(sam,Food) :-
    italian(Food).
likes(sam,chips).

indian(curry).
indian(dahl).
indian(tandoori).%
indian(kurma).

mild(dahl).
mild(tandoori).
mild(kurma).

chinese(chow_mein).
chinese(chop_suey).
chinese(sweet_and_sour).

italian(pizza).
italian(spaghetti).

mildIndian(Food) :-
    indian(Food), mild(Food).

parent( pam, bob).       % Pam is a parent of Bob
parent( tom, bob).
parent( tom, liz).
parent( bob, ann).
parent( bob, pat).
parent( pat, jim).


% Figure 2.16   Four versions of the predecessor program.


% Four versions of the predecessor program

% The original version

pred1( X, Z)  :-
   parent( X, Z).

pred1( X, Z)  :-
   parent( X, Y),
   pred1( Y, Z).

add(X, L,[X|L]).

% add(4, [1,2,3], R).


del(X, [Y| Tail], Tail) :- X = Y.
del(X, [Y| Tail], [Y|Tail1]) :- del( X, Tail, Tail1).

% del(t, [r, s, t, u, v, t], R).
%
position(atdoor).
position(middle).
position(atwindow).

different(A, B) :-
    not(A == B).

newPosition(P1, position(P2)) :-
    position(P1),
    position(P2),
    different(P1, P2).

%% walk(P1, P2) :-
%  position(P2), different(P1, P2).

%%  state(playerPos, elevation, boxPos, bananaStatus
%
%
move( state( middle, onbox, middle, hasnot),
      grasp,
      state( middle, onbox, middle, has)).


move( state( middle, onfloor, middle, H),
      climb,
      state( middle, onbox, middle, H)).


move( state( P1, onfloor, P1, H),
      push(P1, P2),
      state( P2, onfloor, P2, H)).

move( state( P1, onfloor, B, hasnot),
      walk(P1, P2),
      state( P2, onfloor, B, hasnot)).

canget( state(_, _, _, has), []).

%canget( state( atdoor, onfloor, atwindow, hasnot), Actions).
canget( State1, [Move|Moves]) :-
    move( State1, Move, State2), canget( State2, Moves).

