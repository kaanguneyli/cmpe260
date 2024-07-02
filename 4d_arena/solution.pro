% Bilge Kaan Güneyli
% 2020400051
% compiling: yes
% complete: yes


distance(Agent, TargetAgent, Distance):-
	Distance is abs(Agent.x - TargetAgent.x) + abs(Agent.y - TargetAgent.y).


multiverse_distance(StateId, AgentId, TargetStateId, TargetAgentId, Distance):-
	state(StateId, Agents, _, _),
	state(TargetStateId, TargetAgents, _, _),
	history(StateId, UniverseId, Time, _),
	history(TargetStateId, TargetUniverseId, TargetTime, _),
	(Agents.AgentId.class = wizard -> TravelCost = 2; TravelCost = 5),
	Distance is abs(Agents.AgentId.x - TargetAgents.TargetAgentId.x) + abs(Agents.AgentId.y - TargetAgents.TargetAgentId.y) + TravelCost * (abs(Time - TargetTime) + abs(UniverseId - TargetUniverseId)).
	

nearest_agent(StateId, AgentId, NearestAgentId, Distance):-
	state(StateId, Agents, _, _),
	dict_keys(Agents, Ids),
	findall(
		Dist - AgentId1,
		(distance(Agents.AgentId, Agents.AgentId1, Dist), member(AgentId1, Ids), Agents.AgentId.name \= Agents.AgentId1.name),
		Distances
	),
	(length(Distances, Len), Len=0 ->
		Distance is -1, NearestAgentId is -1;
		sort(Distances, [Distance-NearestAgentId|_])
	).


nearest_agent_in_multiverse(StateId, AgentId, TargetStateId, TargetAgentId, Distance):-
	state(StateId, Agents, _, _),
	findall(
		TSId - Dists,
		(
			history(TSId,_,_,_),
			state(TSId, Agents1,_,_),
			findall(
				Dist - TAId,
				(dict_keys(Agents1, Ids1), member(TAId, Ids1), multiverse_distance(StateId, AgentId, TSId, TAId, Dist), Agents.AgentId.name \= Agents1.TAId.name),
				Dists1
			),
			sort(Dists1, Dists)
		),
		Distances
	),
	findall(Dist-X-Y, member(X-[Dist-Y|_], Distances), Dists1),
	findall(Dist-X-Y, member(X-[_,Dist-Y|_], Distances), Dists2),
	append(Dists1, Dists2, Distances1),
	sort(Distances1, [Distance-TargetStateId-TargetAgentId|_]).
	

num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues):-
	state(StateId, Agents, _, _),
	dict_keys(Agents, Ids),
	findall(AgentId, (member(AgentId, Ids), Agents.AgentId.class = warrior, Agents.AgentId.name \= Name), Warriors),
	length(Warriors, NumWarriors),
	findall(AgentId, (member(AgentId, Ids), Agents.AgentId.class = wizard, Agents.AgentId.name \= Name), Wizards),
	length(Wizards, NumWizards),                            
	findall(AgentId, (member(AgentId, Ids), Agents.AgentId.class = rogue, Agents.AgentId.name \= Name), Rogues),
	length(Rogues, NumRogues).
	

difficulty_of_state(StateId, Name, AgentClass, Difficulty):-
	num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues),
	(AgentClass = warrior ->
		Difficulty is 5*NumWarriors + 8*NumWizards + 2*NumRogues;
	AgentClass = wizard ->
		Difficulty is 2*NumWarriors + 5*NumWizards + 8*NumRogues;
		Difficulty is 8*NumWarriors + 2*NumWizards + 5*NumRogues).
		
easiest_traversable_state(StateId, AgentId, TargetStateId):-
	state(StateId, Agents, _, TurnOrder),
	Agent = Agents.get(AgentId),
	history(StateId, UniverseId, Time, _),
	findall(
		Difficulty - TSId,
		(			
			global_universe_id(GlobalUniverseId), universe_limit(UniverseLimit), GlobalUniverseId < UniverseLimit,
			length(TurnOrder, NumAgents), NumAgents > 1,
			current_time(TargetUniverseId, TargetUniCurrentTime, _), TargetTime < TargetUniCurrentTime,
			(Agent.class = wizard -> TravelCost = 2; TravelCost = 5),Cost is abs(TargetTime - Time)*TravelCost + abs(TargetUniverseId - UniverseId)*TravelCost,Agent.mana >= Cost,
			get_earliest_target_state(TargetUniverseId, TargetTime, TSId),state(TSId, TargetAgents, _, TargetTurnOrder),TargetState = state(TSId, TargetAgents, _, TargetTurnOrder),\+tile_occupied(Agent.x, Agent.y, TargetState)
			;
			length(TurnOrder, NumAgents), NumAgents > 1,
			current_time(TargetUniverseId, TargetTime, 0),
			\+(TargetUniverseId = UniverseId),
			(Agent.class = wizard -> TravelCost = 2; TravelCost = 5), Cost is abs(TargetTime - Time)*TravelCost + abs(TargetUniverseId - UniverseId)*TravelCost, Agent.mana >= Cost,
			get_latest_target_state(TargetUniverseId, TargetTime, TSId), state(TSId, TargetAgents, _, TargetTurnOrder),TargetState = state(TSId, TargetAgents, _, TargetTurnOrder),\+tile_occupied(Agent.x, Agent.y, TargetState),

			history(TSId,TargetUniverseId,TargetTime,_), difficulty_of_state(TSId, Agent.name, Agent.class, Difficulty),
			Difficulty > 0
			),
		Difficulties
	),
	%adds the agent's stateid to list
	sort(Difficulties, [Minimum-MinState|_]),
	difficulty_of_state(StateId, Agent.name, Agent.class, Diff),
	(Diff > 0 , Diff =< Minimum ->
		TargetStateId is StateId;
		TargetStateId is MinState
	).

basic_action_policy(StateId, AgentId, Action):-
	state(StateId, Agents, _, _),
	history(StateId, _, Time, _),
	%test each possibiliy in order
	
	%if there is a state to traverse
	(easiest_traversable_state(StateId, AgentId, TargetStateId), TargetStateId \= StateId -> 
		(history(TargetStateId, TargetUniverseId, TargetTime, _),
			TargetTime = Time ->			
				Action = [portal_to_now, TargetUniverseId];
				Action = [portal, TargetUniverseId]
		);
	%elif the agent is warrior
	(Agents.AgentId.class = warrior ->
		(nearest_agent(StateId, AgentId, NearestAgentId, Distance), NearestAgentId \= -1 -> 
			(Distance =< 1 ->
				%attack or move or rest
				Action = [melee_attack, NearestAgentId];
				State = state(StateId, Agents, _, _),
				nearest_agent(StateId, AgentId, NearestAgentIdX, _),
				X1 is Agents.AgentId.x, Y1 is Agents.AgentId.y, X2 is Agents.NearestAgentIdX.x, Y2 is Agents.NearestAgentIdX.y,			
				(X1 < X2, Xn is X1 + 1, \+tile_occupied(Xn, Y1, State) -> Action = [move_right];
				(X1 > X2, Xn is X1 - 1, \+tile_occupied(Xn, Y1, State) -> Action = [move_left];
				(Y1 < Y2, Yn is Y1 + 1, \+tile_occupied(X1, Yn, State) -> Action = [move_up];
				(Y1 > Y2, Yn is Y1 - 1, \+tile_occupied(X1, Yn, State) -> Action = [move_down];
				Action = [rest]
				)))));
		Action = [rest]
		);
	%elif wizard
	(Agents.AgentId.class = wizard -> 
		(nearest_agent(StateId, AgentId, NearestAgentId, Distance), NearestAgentId \= -1 ->
		(Distance =< 10 ->
			Action = [magic_missile, NearestAgentId];
			State = state(StateId, Agents, _, _),
			nearest_agent(StateId, AgentId, NearestAgentIdX, _),
			X1 is Agents.AgentId.x, Y1 is Agents.AgentId.y, X2 is Agents.NearestAgentIdX.x, Y2 is Agents.NearestAgentIdX.y,
			(X1 < X2, Xn is X1 + 1, \+tile_occupied(Xn, Y1, State) -> Action = [move_right];
			(X1 > X2, Xn is X1 - 1, \+tile_occupied(Xn, Y1, State) -> Action = [move_left];
			(Y1 < Y2, Yn is Y1 + 1, \+tile_occupied(X1, Yn, State) -> Action = [move_up];
			(Y1 > Y2, Yn is Y1 - 1, \+tile_occupied(X1, Yn, State) -> Action = [move_down];
			Action = [rest]
			)))));
		Action = [rest]
	);
	%elif rogue
	(Agents.AgentId.class = rogue -> 
		(nearest_agent(StateId, AgentId, NearestAgentId, Distance), NearestAgentId \= -1 ->
		(Distance =< 5 ->
			Action = [ranged_attack, NearestAgentId];
			State = state(StateId, Agents, _, _),
			nearest_agent(StateId, AgentId, NearestAgentIdX, _),
			X1 is Agents.AgentId.x, Y1 is Agents.AgentId.y, X2 is Agents.NearestAgentIdX.x, Y2 is Agents.NearestAgentIdX.y,
			(X1 < X2, Xn is X1 + 1, \+tile_occupied(Xn, Y1, State) -> Action = [move_right];
			(X1 > X2, Xn is X1 - 1, \+tile_occupied(Xn, Y1, State) -> Action = [move_left];
			(Y1 < Y2, Yn is Y1 + 1, \+tile_occupied(X1, Yn, State) -> Action = [move_up];
			(Y1 > Y2, Yn is Y1 - 1, \+tile_occupied(X1, Yn, State) -> Action = [move_down];
			Action = [rest]
			)))));
		Action = [rest]
	))))).
			 
	
	

