-module(pro).
-compile([export_all]).
%% string czy atom...
-record(instr, {opcode = "dat", opmod = "f", amod = '$', a = 0, bmod = '$', b = 0}).

opcodes() ->
	["dat", "mov", "add", "sub", "mul", "div", "mod", "jmp", "jmz", "jmn", "djn", "spl", "cmp", "seq", "sne", "slt", "ldp", "stp", "nop"].
	
instr_mod() ->
	["a", "b", "ab", "ba", "f", "x", "i"].
	
addresinng_modes() ->
	["#", "$", "@", "*", "{", "}", "<", ">"].

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

elo() -> 
	#instr{}.
	
write(A) ->
	A#instr.opcode.
	
open(Filename) ->
	case file:open(Filename, [read]) of
		{ok, Fd} ->
			Fd;
		{error, Reason} ->
			{error, Reason}
	end.
	
read_line(Fd) ->
	case io:get_line(Fd, "") of
		eof ->
			[];
		{error, Reason} ->
			{error, Reason};
		Data ->
			Data
	end.
	
close(Fd) ->
	file:close(Fd).
	
trim(String) ->
	S1 = re:replace(String, "\n", "", [global, {return, list}]),
	S2 = re:replace(S1, "\t", "", [global, {return, list}]),
	string:strip(S2, both).
	
parse_line(Line) when is_list(Line) ->
	L = string:to_lower(trim(Line)),
	Tokens = string:tokens(L, " "),
	Opcode = lists:nth(1, Tokens),
	{Code, Mod} = parse_instruction(string:tokens(Opcode, "."));
	%% dat, nop ignoruja a i b, jmp i spl ignoruja b
parse_line(_) ->
	error.
	
parse_instruction([Opcode]) ->
	case is_opcode(Opcode) of
		true	-> {Opcode, element(3, lists:keyfind(Opcode, 2, instructions()))};
		false	-> throw("Illegal opcode")
	end;

parse_instruction([Opcode, Mod]) ->
	case is_opcode(Opcode) of
		true ->
			case is_instr_mod(Mod) of
				true 	-> {Opcode, Mod};
				false	-> throw("Illegal instruction modifier")
			end;
		false ->
			throw("Illegal opcode")
	end.
	
%%check_instr_mod(Code) ->
is_opcode(Code) ->
	lists:member(Code, opcodes()).
	
is_instr_mod(Mod) ->
	lists:member(Mod, instr_mod()).
	
%ge%t_instruction(Code) ->
	%lists:keyfind(Code, 1, instructions()).
	


%%sprawdzenie czy instruckja jest odpowiednia
%%sparsowanie modyfikatora instrukcji
%%sparsowanie modyfikatora A i wartości
%%sprasowanie modyfikatora B i wartości