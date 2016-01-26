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
	receive
		{Pid, load_warrior, Filename} ->
			io:format("sPrzyszlo~n"),
			io:format("sposzlo~n"),
			try parser:parse_file(Filename) of
				Instructions ->
					try add_warrior(Core, Instructions) of
						{NewCore}->%, WarriorPid} ->
							Pid ! {self(), ok},
							loop(NewCore, Warriors)%[WarriorPid | Warriors])
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
			loop(Core, [])

	end.

%% ok
load_warrior(Pid, Filename)	->
	Pid ! {self(), load_warrior, Filename},
	receive
		{Pid, Message} ->
			Message
	end.

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
	NewNumber = number_of_warriors(Core) + 1,
	case NewNumber of
		8 ->
			throw("Maximum number of warrios have been reached");
		_ ->
			Location = new_warrior_location(Core, length(Instructions)),
			{add_warrior(Core, Instructions, Location, NewNumber)}%, spawn(warrior, loop, [Location])}
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

delete_warrior([Head = {warrior, Num, Pid} | Rest], Number, Accum) ->
	case Num =:= Number of
		false ->
			delete_warrior(Rest, Number, [Head | Accum]);
		true ->
			Pid ! {self(), kill},
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
	case scan_for_other_warriors(Core, 1, length(Core), 0) of
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