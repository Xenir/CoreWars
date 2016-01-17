-module(server).
-compile([export_all]).
-record(cell, {instr = parser:default_instr(), warrior = 0}).

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
maximum_warriors() ->
	length(core_chars()) - 1.

%% ok
init() ->
	init(800).

%% ok
init(Size) ->
	spawn(?MODULE, loop, [lists:duplicate(Size, #cell{}), []]).

%% testowane
loop(Core, Warriors) ->
	receive
		{Pid, load_warrior, Filename} ->
			add_warrior(Core, parser:parse_file(Filename)),
			Pid ! {self(), ok}
			%try parser:parse_file(Filename) of
			%	Instructions ->
			%		try add_warrior(Core, Instructions) of
			%			{NewCore, WarriorPid} ->
			%				Pid ! {self(), ok},
			%				loop(NewCore, [WarriorPid | Warriors])
			%		catch
			%			Reason ->
			%				Pid ! {self(), Reason},
			%				loop(Core, Warriors)
			%		end
			%catch
			%	Reason ->
			%		Pid ! {self(), Reason},
			%		loop(Core, Warriors)
			%end

		%{Pid, start} ->

	end.

%% testowane
load_warrior(Pid, Filename)	->
	Pid ! {self(), load_warrior, Filename},
	receive
		{_, Message} ->
			Message
	end.

%% ok
cell_to_char(Cell) ->
	lists:nth(element(3, Cell) + 1, core_chars()).

%% ok
nth_cell(N, Core) ->
	lists:nth(N rem length(Core) + 1, Core).

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

%% ok
add_warrior(Core, Instructions, Location, WarriorNumber) ->
	subcore(Core, 1, Location - 1) ++ instructions_to_cells(Instructions, WarriorNumber) ++ subcore(Core, Location + length(Instructions), length(Core)).

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
	extract_warriors(CoreTail, [element(3, CoreHead) | Accum]).

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
scan_for_other_warriors(Core, Begin, End, Warrior) -> aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
	lists:any(fun(Cell) -> Cell =/= Warrior end, extract_warriors(subcore(Core, Begin, End))).

%% ok
empty(Cell) ->
	element(3, Cell) =:= 0.

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