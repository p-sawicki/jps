start_A_star( InitState, MaxSteps, PathCost) :-
	score(InitState, 0, 0, InitCost, InitScore) ,
	search_A_star( [node(InitState, nil, nil, InitCost , InitScore ) ], [ ], MaxSteps, 1, PathCost) .


search_A_star(Queue, ClosedSet, MaxSteps, Step, PathCost) :-
	write('Krok '), write(Step), nl,

	fetch(Node, Queue, ClosedSet, MaxSteps, RestQueue, RestPrio),
	continue(Node, RestQueue, ClosedSet, MaxSteps, Step, RestPrio, PathCost).


continue(node(State, Action, Parent, Cost, _), _, ClosedSet, _, _, _, path_cost(Path, Cost) ) :-
	goal( State), !,
	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .

continue(Node, RestQueue, ClosedSet, MaxSteps, Step, _, Path)   :-
	Step < MaxSteps, !,
	expand( Node, NewNodes),
	insert_new_nodes(NewNodes, RestQueue, NewQueue),
	NextStep is Step + 1,
	search_A_star(NewQueue, [Node | ClosedSet ], MaxSteps, NextStep, Path).

continue(Node, RestQueue, ClosedSet, MaxSteps, Step, [], Path) :-
	Step >= MaxSteps, 
	get_answer(A), A = tak, !,
	NewMaxSteps is MaxSteps + 1,
	continue(Node, RestQueue, ClosedSet, NewMaxSteps, Step, [], Path).


fetch(node(State, Action,Parent, Cost, Score), Queue, _, MaxSteps, RestQueue, RestPrio) :-
	write_q(Queue, MaxSteps), nl,
	len(Queue, MaxSteps, QLength), get_prio(Queue, QLength, Prio), !,
	get_from_prio(node(State, Action,Parent, Cost, Score), Prio, RestPrio),

	write('fetch '), write(State), nl,
	del(Queue, node(State, _, _, _, _), RestQueue).

% retrieves nodes in order of ascending priority
get_from_prio(Node, [pair(_, Node) | RestPrio], RestPrio).

get_from_prio(Node, [_ | Rest], RestPrio) :- get_from_prio(Node, Rest, RestPrio).


expand(node(State, _ ,_ , Cost, _ ), NewNodes)  :-
	findall(node(ChildState, Action, State, NewCost, ChildScore) ,
			(succ(State, Action, StepCost, ChildState),
			    score(ChildState, Cost, StepCost, NewCost, ChildScore)),
			NewNodes).


score(State, ParentCost, StepCost, Cost, FScore)  :-
	Cost is ParentCost + StepCost ,
	hScore(State, HScore),
	FScore is Cost + HScore.


insert_new_nodes( [ ], Queue, Queue).

insert_new_nodes( [Node|RestNodes], Queue, NewQueue) :-
	insert_p_queue(Node, Queue, Queue1),
	insert_new_nodes( RestNodes, Queue1, NewQueue).


insert_p_queue(Node, [ ], [Node]) :- !.

insert_p_queue(node(State, Action, Parent, Cost, FScore),
		[node(State1, Action1, Parent1, Cost1, FScore1)|RestQueue],
			[node(State1, Action1, Parent1, Cost1, FScore1)|Rest1] )  :-
	FScore >= FScore1,  ! ,
	insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1).

insert_p_queue(node(State, Action, Parent, Cost, FScore),  Queue,
				[node(State, Action, Parent, Cost, FScore)|Queue]).


build_path(node(nil, _, _, _, _), _, Path, Path) :- !.

build_path(node(EndState, _, _, _, _), Nodes, PartialPath, Path) :-
	del(Nodes, node(EndState, Action, Parent, _, _), Nodes1) ,
	build_path( node(Parent,_ ,_ , _ , _ ) , Nodes1, [Action/EndState|PartialPath],Path).


del([X|R],X,R).
del([Y|R],X,[Y|R1]) :-
	X\=Y,
	del(R,X,R1).

% prints states of up to `Length` nodes in given list
write_q([], _).

write_q(_, 0).

write_q([node(State, _, _, _, _)|Rest], Length) :- 
	write(State), 
	NextLength is Length - 1, 
	write_q(Rest, NextLength).

% get max{length of liven list, `MaxLength`}
len([], _, 0).

len(_, 0, 0).

len([_ | Rest], MaxLength, Length) :- 
	NextLength is MaxLength - 1, 
	len(Rest, NextLength, RestLength), 
	Length is RestLength + 1.

% insert pair (Priority, Node) into list placed such that ascending order by Priority is kept
insert(NewPair, [], [NewPair]) :- !.

insert(pair(NewPrio, NewNode), [pair(FirstPrio, FirstNode) | Rest], [pair(NewPrio, NewNode), pair(FirstPrio, FirstNode) | Rest]) :- 
	NewPrio =< FirstPrio, !.

insert(pair(NewPrio, NewNode), [pair(FirstPrio, FirstNode) | Rest], [pair(FirstPrio, FirstNode) | Inserted]) :- 
	NewPrio > FirstPrio, 
	insert(pair(NewPrio, NewNode), Rest, Inserted).

% get list of length `Length` of pairs (Priority, Node) in ascending order by Priority where Priority is given by user
get_prio(_, 0, []).

get_prio([Node|Q], Length, Result) :- 
	read(Priority), 
	NextLength is Length - 1, 
	get_prio(Q, NextLength, Rest), 
	insert(pair(Priority, Node), Rest, Result).

get_answer(A) :- print('Wyczerpano limit krokow. Zwiekszyc? (tak/nie): '), read(A).

succ(a, ab, 2, b).
succ(b, bf, 3, f).
succ(a, ac, 3, c).
succ(b, bg, 4, g).
succ(g, gm, 2, m).
succ(c, cd, 2, d).
succ(d, dm, 2, m).
succ(c, ce, 3, e).
succ(e, em, 5, m).

goal(m).

hScore(a, 4).
hScore(b, 4).
hScore(f, 7).
hScore(g, 1).
hScore(m, 0).
hScore(c, 3).
hScore(d, 1).
hScore(e, 4).
