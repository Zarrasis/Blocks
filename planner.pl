
%%%%%%%%% Simple Prolog Planner %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Based on one of the sample programs in:
%%%
%%% Artificial Intelligence:
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Two Room Planner by Matthew Taubler and Brooke Norton


%add test(x)/0
:- module( planner,
	   [
	       plan/4,change_state/3,conditions_met/2,member_state/2,
	       move/3,go/2,test/0,test2/0,test3/0,test4/0
	   ]).

:- [utils].

% if the sets state and goal are equal, we can print the moves
plan(State, Goal, _, Moves) :-	equal_set(State, Goal),
				write('moves are'), nl,
				reverse_print_stack(Moves).

%no idea, i think this is where we might IDFS
plan(State, Goal, Been_list, Moves) :-
				move(Name, Preconditions, Actions),
				conditions_met(Preconditions, State),
				change_state(State, Actions, Child_state),
				not(member_state(Child_state, Been_list)),
				stack(Child_state, Been_list, New_been_list),
				stack(Name, Moves, New_moves),
			plan(Child_state, Goal, New_been_list, New_moves),!.

change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new), !.
conditions_met(P, S) :- subset(P, S).

member_state(S, [H|_]) :-	equal_set(S, H).
member_state(S, [_|T]) :-	member_state(S, T).

/* move types */

% pickup off another block
% see if X is on block Y in room R, also if hand is in room R
move(pickup(X), [handempty, clear(X), on(X, Y, R), handroom(R)],
		[del(handempty), del(clear(X)), del(on(X, Y, R)),
				 add(clear(Y)),	add(holding(X))]).

% pick up from table
% see if x is on the table in room R, and hand is in room R
move(pickup(X), [handempty, clear(X), ontable(X,R), handroom(R)],
		[del(handempty), del(clear(X)), del(ontable(X,R)),
				 add(holding(X))]).

% put down on table
% if you are in the correct room R add X to the table in that room R
move(putdown(X), [holding(X), handroom(R)],
		[del(holding(X)), add(ontable(X,R)), add(clear(X)),
				  add(handempty)]).

% put down on stack
% if you are holding x and block y is clear and you are in room r, then
% add on to that block, remove y from clear and stack x on y in r
move(stack(X, Y), [holding(X), clear(Y), handroom(R)],
		[del(holding(X)), del(clear(Y)), add(handempty), add(on(X,Y,R)),
				  add(clear(X))]).

% go to room 1
move(goroom(1),
	[handroom(2)], [del(handroom(2)),
		add(handroom(1))]).

% go to room 2
move(goroom(2),
	[handroom(1)],	[del(handroom(1)),
		add(handroom(2))]).

/* run commands */

go(S, G) :- plan(S, G, [S], []).

test :- go([handempty, ontable(b, 1), ontable(c,1), on(a, b, 1), clear(c), clear(a), handroom(1)],
	          [handempty, ontable(c, 1), on(a, b, 1), on(b, c, 1), clear(a), handroom(1)]).

test2 :- go([handempty, ontable(b, 1), ontable(c, 1), on(a, b, 1), clear(c), clear(a), handroom(1)],
	          [handempty, ontable(a, 1), ontable(b, 1), on(c, b, 1), clear(a), clear(c), handroom(1)]).

test3 :- go([handempty, ontable(d,1), ontable(c,1), on(c,b,1), on(b,a,1), clear(d), clear(a), handroom(1)],
						[handempty, ontable(d,2), on(d,c,1), on(c,b,1), on(b,a,1), clear(a), handroom(2)]).

test4 :- go([handempty, ontable(b, 1), on(a, b, 1), clear(a), handroom(1)],
							[handempty, ontable(b, 2), on(a, b, 2), clear(a),  handroom(1)]).
