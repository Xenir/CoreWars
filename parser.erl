-module(parser).
-export([parse_file/1, default_instr/0]).
-record(instr, {opcode = "dat", opmod = "f", amod = '$', a = 0, bmod = '$', b = 0}).

%% ok
opcodes() ->
	["dat", "mov", "add", "sub", "mul", "div", "mod", "jmp", "jmz", "jmn", "djn", "spl", "cmp", "seq", "sne", "slt", "ldp", "stp", "nop"].

%% ok
instr_mod() ->
	["a", "b", "ab", "ba", "f", "x", "i"].

%% ok
addressing_modes() ->
	["#", "$", "@", "*", "{", "}", "<", ">"].

%% ok
default_addresing_mode() ->
	"$".

%% ok
default_instr() ->
	#instr{}.

%% ok
instructions() ->
	[
		#instr{opcode = "dat", opmod = "f", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "mov", opmod = "i", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "add", opmod = "f", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "sub", opmod = "f", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "mul", opmod = "f", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "div", opmod = "f", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "mod", opmod = "f", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "jmp", opmod = "b", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "jmz", opmod = "b", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "jmn", opmod = "b", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "djn", opmod = "b", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "spl", opmod = "b", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "cmp", opmod = "i", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "seq", opmod = "i", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "sne", opmod = "i", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "slt", opmod = "b", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "ldp", opmod = "b", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "stp", opmod = "b", amod = '$', a = 0, bmod = '$', b = 0},
		#instr{opcode = "nop", opmod = "f", amod = '$', a = 0, bmod = '$', b = 0}
	].

%% ok
open_file(Filename) ->
	case file:open(Filename, [read]) of
		{ok, Fd} ->
			Fd;
		{error, Reason} ->
			throw(Reason)
	end.

%% ok
read_line(Fd) ->
	case io:get_line(Fd, "") of
		eof ->
			[];
		{error, Reason} ->
			{error, Reason};
		Data ->
			Data
	end.

%% ok
close_file(Fd) ->
	file:close(Fd).

%% ok
trim(String) ->
	S1 = re:replace(String, "\n", "", [global, {return, list}]),
	S2 = re:replace(S1, "\t", "", [global, {return, list}]),
	string:strip(S2, both).

%% ok
parse_line([]) ->
	throw("Empty line");

%% ok
parse_line(Line) when is_list(Line) ->
	L = string:to_lower(trim(Line)),
	Tokens = string:tokens(L, " "),
	{Code, Mod} = parse_instruction(string:tokens(lists:nth(1, Tokens), ".")),
	{Amod, A} = parse_field(lists:nth(2, Tokens)),
	{Bmod, B} = parse_field(lists:nth(3, Tokens)),
	create_instruction(Code, Mod, Amod, A, Bmod, B);

%% ok
parse_line(_) ->
	throw("Not a string").

%% ok
parse_instruction([Opcode]) ->
	case is_opcode(Opcode) of
		true	-> {Opcode, element(3, lists:keyfind(Opcode, 2, instructions()))};
		false	-> throw("Illegal opcode: " ++ Opcode)
	end;

%% ok
parse_instruction([Opcode, Mod]) ->
	case is_opcode(Opcode) of
		true ->
			case is_instr_mod(Mod) of
				true 	-> {Opcode, Mod};
				false	-> throw("Illegal instruction modifier: " ++ Mod)
			end;
		false ->
			throw("Illegal opcode: " ++ Opcode)
	end.

%% ok
parse_field(Field) when is_list(Field) ->
	First = string:substr(Field, 1, 1),
	case is_addressing_mode(First) of
		true ->
			Data = string:substr(Field, 2),
			Mode = First;
		false ->
			Data = Field,
			Mode = default_addresing_mode()
	end,
	case string:to_integer(Data) of
		{error, _} ->
			throw("Cannot parse the addres: " + Field);
		{Int, _} ->
			{Mode, Int}
	end.

%% ok
is_opcode(Code) ->
	lists:member(Code, opcodes()).

%% ok
is_instr_mod(Mod) ->
	lists:member(Mod, instr_mod()).

%% ok
is_addressing_mode(Mode) ->
	lists:member(Mode, addressing_modes()).

%% ok
create_instruction(Code, Mod, Amod, A, Bmod, B) ->
	#instr{opcode = Code, opmod = Mod, amod = Amod, a = A, bmod = Bmod, b = B}.

%% ok
parse_file(Filename) ->
	Fd = open_file(Filename),
	Instructions = parse_file(Fd, []),
	close_file(Fd),
	Instructions.

%% ok
parse_file(Fd, Accum) ->
	case read_line(Fd) of
		[] ->
			Accum;
		{error, Reason} ->
			throw(Reason);
		Line ->
			parse_file(Fd, Accum ++ [parse_line(Line)])
	end.