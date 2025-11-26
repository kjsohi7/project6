% search.pl
% No module — required by the autograder

:- use_module(library(lists)).

% ------------------------------
% State definition
% ------------------------------
% A state is represented as:
%   state(CurrentRoom, KeysHeld)

% ------------------------------
% Entry point:
% search(Actions)
% ------------------------------

search(Actions) :-
    initial(StartRoom),
    bfs([node(state(StartRoom, []), [])], [], RevActions),
    reverse(RevActions, Actions).

% ------------------------------------------------
% BFS implementation
% ------------------------------------------------

% Case 1: Front of queue reaches the treasure
bfs([node(state(Room, _Keys), ActionsSoFar) | _], _Visited, ActionsSoFar) :-
    treasure(Room), !.

% Case 2: Expand next node
bfs([node(State, ActionsSoFar) | RestQueue], Visited, Actions) :-
    State = state(_, _),
    findall(
        node(NextState, [Act | ActionsSoFar]),
        move(State, NextState, Act),
        Children
    ),
    add_new_states(Children, Visited, NewNodes, NewVisited),
    append(RestQueue, NewNodes, UpdatedQueue),
    bfs(UpdatedQueue, NewVisited, Actions).

% Case 3: Queue empty → fail
bfs([], _, _) :-
    fail.

% ------------------------------------------------
% Visited-state filtering
% ------------------------------------------------

add_new_states([], Visited, [], Visited).

add_new_states([node(State, Acts) | Rest], Visited,
               [node(State, Acts) | NewNodes], NewVisited) :-
    \+ member(State, Visited),
    add_new_states(Rest, [State | Visited], NewNodes, NewVisited).

add_new_states([node(State, _) | Rest], Visited, NewNodes, NewVisited) :-
    member(State, Visited),
    add_new_states(Rest, Visited, NewNodes, NewVisited).

% ------------------------------------------------
% Movement rules
% ------------------------------------------------

% Normal door (A -> B)
move(state(Room, Keys), state(NextRoom, Keys2), move(Room, NextRoom)) :-
    door(Room, NextRoom),
    pick_up_key(NextRoom, Keys, Keys2).

% Normal door (B -> A)
move(state(Room, Keys), state(NextRoom, Keys2), move(Room, NextRoom)) :-
    door(NextRoom, Room),
    pick_up_key(NextRoom, Keys, Keys2).

% Locked door (A -> B)
move(state(Room, Keys), state(NextRoom, Keys2), move(Room, NextRoom)) :-
    locked_door(Room, NextRoom, Color),
    member(Color, Keys),
    pick_up_key(NextRoom, Keys, Keys2).

% Locked door (B -> A)
move(state(Room, Keys), state(NextRoom, Keys2), move(Room, NextRoom)) :-
    locked_door(NextRoom, Room, Color),
    member(Color, Keys),
    pick_up_key(NextRoom, Keys, Keys2).

% ------------------------------------------------
% Key pickup
% ------------------------------------------------

pick_up_key(Room, KeysHeld, NewKeysHeld) :-
    key(Room, Color),
    \+ member(Color, KeysHeld),
    sort([Color | KeysHeld], NewKeysHeld), !.

pick_up_key(_, Keys, Keys).
