-module(server).
-compile([export_all]).
-record(cell, {instr = parser:default_instr(), warrior = 0}).
-record(warrior, {number = 0, pid}).

%% ok
min_dist() ->
	10.

%% ok
core_chars() ->
	[
	".",	% empty core
	"!",	% 1st warrior
	"@",	% 2nd warrior
	"#",	% 3rd warrior
	"$",	% 4th warrior
	"%",	% 5th warrior
	"+",	% 6th warrior
	"-",	% 7th warrior
	"&"		% 8th warrior
	].

%% ok
create_cell(Instr, WarriorNum) ->
	#cell{instr = Instr, warrior = WarriorNum}.

%% ok
maximum_warriors() ->
	length(core_chars()) - 1.

%% ok
init() ->
	init(800).

%% ok
init(Size) ->
	spawn(?MODULE, loop, [lists:duplicate(Size, #cell{}), []]).

%% odkomentowac
loop(Core, Warriors) ->
	io:format("~n~n~n~s~n~n~n", ["loop"]),
	receive
		{Pid, load_warrior, Filename} ->
			io:format("przyszlo~n"),
			try parser:parse_file(Filename) of
				Instructions ->
					io:format("sparsowane~n"),
					try add_warrior(Core, Instructions) of
						{NewCore, Number, WarriorPid} ->
							Pid ! {self(), ok},
							io:format("wyslane~n"),
							loop(NewCore, [#warrior{number = Number, pid = WarriorPid} | Warriors])
					catch
						Reason ->
							Pid ! {self(), Reason},
							loop(Core, Warriors)
					end
			catch
				Reason ->
					Pid ! {self(), Reason},
					loop(Core, Warriors)
			end;

		{Pid, remove_warrior, WarriorNum} ->
			W = delete_warrior(Warriors, WarriorNum),
			Pid ! {self(), ok},
			loop(Core, W);

		{Pid, remove_all} ->
			Pid ! {self(), ok},
			loop(Core, []);
			
		{Pid, start, Steps} ->
			Pid ! {self(), launching},
			{NCore, NWarriors} = start_battle(Core, Warriors, Steps),
			loop(NCore, NWarriors);
		
		{Pid, stop} ->
			Pid ! {self(), stopping}

	end.

run(Pid) ->
	run(Pid, 8000).

run(Pid, Steps) ->
	Pid ! {self(), start, Steps},
	receive
		{Pid, Message} ->
			Message
	end.

stop(Pid) ->
	Pid ! {self(), stop},
	receive
		{Pid, Message} ->
			Message
	end.
	
start_battle(Core, Warriors, 0) ->
	write_core(Core),
	io:format("Battle ended with tie: ~w warriors alive~n", [length(Warriors)]),
	{Core, Warriors};
	
start_battle(Core, Warriors, Steps) ->
	io:format("~w~n", [Warriors]),
	case Steps rem 20 =:= 0 of
		true ->
			write_core(Core)
	end,
	io:format("Steps: ~w~n", [Steps]),
	WNum = Steps rem length(Warriors) + 1,
	War = lists:nth(WNum, Warriors),
	case execute_current_instr(Core, War) of
		{ok, NewCore} ->
			start_battle(NewCore, Warriors, Steps - 1);
		{dead, NewCore} ->
			NewWarriors = delete_warrior(Warriors, WNum),
			case length(NewWarriors) of
				1 ->
					write_core(Core),
					io:format("Warrior '~w' has won the battle!~n", [lists:nth((lists:nth(1, NewWarriors))#warrior.number, core_chars())]);
				_ ->
					start_battle(NewCore, NewWarriors, Steps - 1)
			end
	end.
	
execute_current_instr(Core, Warrior) ->
	WPid = Warrior#warrior.pid,
	WPid ! {self(), execute, Core},
	receive
		{WPid, Status, NewCore} ->
			{Status, NewCore}
	end.
	
%% ok
load_warrior(Pid, Filename)	->
	Pid ! {self(), load_warrior, Filename},
	receive
		{Pid, Message} ->
			io:format("odebrane ~w", [Message])
	end,
	io:format("wyszło ~w", [Pid]).

%% do testow
remove_warrior(Pid, WarriorNumber) when WarriorNumber > 0 ->
	Pid ! {self(), remove_warrior, WarriorNumber},
	receive
		{Pid, Message} ->
			Message
	end.

%% testowanie
remove_all(Pid) ->
	Pid ! {self(), remove_all},
	receive
		{Pid, Message} ->
			Message
	end.

%% ok
cell_to_char(Cell) ->
	lists:nth(extract_warrior(Cell) + 1, core_chars()).

write_cell(Cell) ->
	io:format("~s", [cell_to_char(Cell)]).

write_core(Core) ->
	write_core(Core, 0).

write_core([], _) ->
	io:format("~n");

write_core(Core, 80) ->
	io:format("~n"),
	write_core(Core, 0);

write_core([Head | Tail], N) ->
	write_cell(Head),
	write_core(Tail, N + 1).

%% ok
nth_cell(N, Core) ->
	lists:nth(N rem length(Core) + 1, Core).

% ok
extract_instr(Cell) ->
	Cell#cell.instr.

%% odkomentowac
add_warrior(Core, Instructions) ->
	io:format("dodawwnie~n"),
	NewNumber = number_of_warriors(Core) + 1,
	case NewNumber of
		9 ->
			throw("Maximum number of warrios have been reached");
		_ ->
			Location = new_warrior_location(Core, length(Instructions)),
			{add_warrior(Core, Instructions, Location, NewNumber), NewNumber, spawn(warrior, loop, [Location])}
	end.

replace_cell(Core, N, NewCell) ->
	subcore(Core, 1, N - 1) ++ [NewCell] ++ subcore(Core, N + 1, length(Core)).

%% ok
add_warrior(Core, Instructions, Location, WarriorNumber) ->
	subcore(Core, 1, Location - 1) ++ instructions_to_cells(Instructions, WarriorNumber) ++ subcore(Core, Location + length(Instructions), length(Core)).

%% testowanie
delete_warrior(Warriors, Number) when is_list(Warriors) ->
	lists:reverse(delete_warrior(Warriors, Number, [])).

delete_warrior([], _, Accum) ->
	Accum;

delete_warrior([Head | Rest], Number, Accum) ->
	case Head#warrior.number =:= Number of
		false ->
			delete_warrior(Rest, Number, [Head | Accum]);
		true ->
			Head#warrior.pid ! {self(), kill},
			delete_warrior(Rest, Number, Accum)
	end.

%% ok
instructions_to_cells(Instructions, WarriorNumber) ->
	lists:reverse(instructions_to_cells(Instructions, WarriorNumber, [])).

%% ok
instructions_to_cells([], _, Accum) ->
	Accum;

%% ok
instructions_to_cells([Head | Tail], WarriorNumber, Accum) ->
	instructions_to_cells(Tail, WarriorNumber, [instr_to_cell(Head, WarriorNumber) | Accum]).

%% ok
instr_to_cell(Instruction, WarriorNumber) ->
	#cell{instr = Instruction, warrior = WarriorNumber}.

%% ok
number_of_warriors(Core) ->
	lists:max(extract_warriors(Core)).

%% ok
extract_warriors(Core) ->
	extract_warriors(Core, []).

%% ok
extract_warriors([], Accum) ->
	Accum;

%% ok
extract_warriors([CoreHead | CoreTail], Accum) ->
	extract_warriors(CoreTail, [extract_warrior(CoreHead) | Accum]).

% ok
extract_warrior(Cell) ->
	Cell#cell.warrior.

%% ok
new_warrior_location(Core, WarriorLength) ->
	Location = random:uniform(length(Core) - WarriorLength + 1),
	io:format("~w~n", [Location]),
	case scan_for_other_warriors(Core, Location, Location + WarriorLength, 0) of
		true ->
			new_warrior_location(Core, WarriorLength);
		false ->
			Location
	end.

%% ok
subcore(Core, Begin, End) ->
	lists:sublist(Core, Begin, End - Begin + 1).

%% ok
scan_for_other_warriors(Core, Begin, End, Warrior) ->
	lists:any(fun(Cell) -> Cell =/= Warrior end, extract_warriors(subcore(Core, Begin, End))).

%% ok
empty(Cell) ->
	extract_warrior(Cell) =:= 0.

%% ok
empty_space(Cell, Accum) ->
	case empty(Cell) of
		false ->
			Accum;
		true ->
			Accum ++ [length(Accum) + 1]
	end.

%% ok
find_empty_space(Core) ->
	lists:foldl(fun(Cell, Accum) -> empty_space(Cell, Accum) end, [], Core).