-module(mech).
-compile([export_all]).

execute(Core, N) ->
	case parser:extract_opcode(server:extract_instr(server:nth_cell(N, Core))) of
		"dat" ->
			execute_dat(Core, N);
		"mov" ->
			execute_mov(Core, N);
		"add" ->
			execute_add(Core, N);
		"sub" ->
			execute_sub(Core, N);
		"mul" ->
			execute_mul(Core, N);
		"div" ->
			execute_div(Core, N);
		"mod" ->
			execute_mod(Core, N);
		"jmp" ->
			execute_jmp(Core, N);
		"jmz" ->
			execute_jmz(Core, N);
		"jmn" ->
			execute_jmn(Core, N);
		"djn" ->
			execute_djn(Core, N);
		"spl" ->
			execute_spl(Core, N);
		"cmp" ->
			execute_cmp(Core, N);
		"seq" ->
			execute_seq(Core, N);
		"sne" ->
			execute_sne(Core, N);
		"slt" ->
			execute_slt(Core, N);
		"ldp" ->
			execute_ldp(Core, N);
		"stp" ->
			execute_stp(Core, N);
		"nop" ->
			execute_nop(Core, N)
	end.
	
execute_dat(Core, N) ->
	{kill, dat, Core, N}.

execute_mov(Core, N) ->
	SAddr = get_source_address(Core, N),
	DAddr = get_destination_address(Core, N),
	{Status, Cell} = execute_move(server:nth_cell(SAddr, Core), server:nth_cell(DAddr, Core)),
	NewCore = server:replace_cell(Core, DAddr, Cell),
	{Status, "mov", NewCore, N + 1}.

execute_add(Core, N) ->
	SAddr = get_source_address(Core, N),
	DAddr = get_destination_address(Core, N),
	{Status, Cell} = execute_arithm(server:nth_cell(SAddr, Core), server:nth_cell(DAddr, Core), fun(A, B) -> A + B end),
	NewCore = server:replace_cell(Core, DAddr, Cell),
	{Status, "add", NewCore, N + 1}.

execute_sub(Core, N) ->
	SAddr = get_source_address(Core, N),
	DAddr = get_destination_address(Core, N),
	{Status, Cell} = execute_arithm(server:nth_cell(SAddr, Core), server:nth_cell(DAddr, Core), fun(A, B) -> A - B end),
	NewCore = server:replace_cell(Core, DAddr, Cell),
	{Status, "sub", NewCore, N + 1}.
	
execute_mul(Core, N) ->
	SAddr = get_source_address(Core, N),
	DAddr = get_destination_address(Core, N),
	{Status, Cell} = execute_arithm(server:nth_cell(SAddr, Core), server:nth_cell(DAddr, Core), fun(A, B) -> A * B end),
	NewCore = server:replace_cell(Core, DAddr, Cell),
	{Status, "mul", NewCore, N + 1}.
	
execute_div(Core, N) ->
	SAddr = get_source_address(Core, N),
	DAddr = get_destination_address(Core, N),
	{Status, Cell} = execute_arithm(server:nth_cell(SAddr, Core), server:nth_cell(DAddr, Core), fun(A, B) -> A div B end),
	NewCore = server:replace_cell(Core, DAddr, Cell),
	{Status, "div", NewCore, N + 1}.
	
execute_mod(Core, N) ->
	SAddr = get_source_address(Core, N),
	DAddr = get_destination_address(Core, N),
	{Status, Cell} = execute_arithm(server:nth_cell(SAddr, Core), server:nth_cell(DAddr, Core), fun(A, B) -> A rem B end),
	NewCore = server:replace_cell(Core, DAddr, Cell),
	{Status, "mod", NewCore, N + 1}.
	
execute_jmp(Core, N) ->
	{Status, Addr} = execute_unconditional_jump(Core, N),
	{Status, "jmp", Core, Addr}.
	
execute_jmz(Core, N) ->
	execute_nop(Core, N).
	
execute_jmn(Core, N) ->
	execute_nop(Core, N).
	
execute_djn(Core, N) ->
	execute_nop(Core, N).
	
execute_spl(Core, N) ->
	execute_jmp(Core, N).
	
execute_cmp(Core, N) ->
	execute_seq(Core, N).
	
execute_seq(Core, N) ->
	execute_nop(Core, N).

execute_sne(Core, N) ->
	execute_nop(Core, N).
	
execute_slt(Core, N) ->
	execute_nop(Core, N).
	
execute_ldp(Core, N) ->
	execute_nop(Core, N).

execute_stp(Core, N) ->
	execute_nop(Core, N).

execute_nop(Core, N) ->
	{ok, "nop", Core, N + 1}.
	
execute_arithm(Source, Destination, Fun) ->
	SourceInstr = server:extract_instr(Source),
	DestInstr = server:extract_instr(Destination),
	InstrMod = parser:extract_opmod(SourceInstr),
	case InstrMod of
		"a" ->
			SA = parser:extract_a(SourceInstr),
			DA = parser:extract_a(DestInstr),
			RA = Fun(DA, SA),
			RB = parser:extract_b(DestInstr);
		"b" ->
			SB = parser:extract_b(SourceInstr),
			DB = parser:extract_b(DestInstr),
			RA = parser:extract_a(DestInstr),
			RB = Fun(DB, SB);
		"ab" ->
			SA = parser:extract_a(SourceInstr),
			DB = parser:extract_b(DestInstr),
			RA = parser:extract_a(DestInstr),
			RB = Fun(DB, SA);
		"ba" ->
			SB = parser:extract_b(SourceInstr),
			DA = parser:extract_a(DestInstr),
			RA = Fun(DA, SB),
			RB = parser:extract_b(DestInstr);
		"f" ->
			SA = parser:extract_a(SourceInstr),
			SB = parser:extract_b(SourceInstr),
			DA = parser:extract_a(DestInstr),
			DB = parser:extract_b(DestInstr),
			RA = Fun(DA, SA),
			RB = Fun(DB, SB);
		"x" ->
			SA = parser:extract_a(SourceInstr),
			SB = parser:extract_b(SourceInstr),
			DA = parser:extract_a(DestInstr),
			DB = parser:extract_b(DestInstr),
			RA = Fun(DA, SB),
			RB = Fun(DB, SA);
		"i" ->
			SA = parser:extract_a(SourceInstr),
			SB = parser:extract_b(SourceInstr),
			DA = parser:extract_a(DestInstr),
			DB = parser:extract_b(DestInstr),
			RA = Fun(DA, SA),
			RB = Fun(DB, SB)
	end,
	RCode = parser:extract_opcode(DestInstr),
	RMode = parser:extract_opmod(DestInstr),
	RAmod = parser:extract_amod(DestInstr),
	RBmod = parser:extract_bmod(DestInstr),
	{ok, server:create_cell(parser:create_instr(RCode, RMode, RAmod, RA, RBmod, RB), server:extract_warrior(Source))}.
	
execute_division(Source, Destination, Fun) ->
	SourceInstr = server:extract_instr(Source),
	DestInstr = server:extract_instr(Destination),
	InstrMod = parser:extract_opmod(SourceInstr),
	case InstrMod of
		"a" ->
			SA = parser:extract_a(SourceInstr),
			DA = parser:extract_a(DestInstr),
			RB = parser:extract_b(DestInstr),
			case SA of
				0 ->
					RA = DA,
					Status = kill;
				true ->
					RA = Fun(DA, SA),
					Status = ok
			end;
		"b" ->
			SB = parser:extract_b(SourceInstr),
			DB = parser:extract_b(DestInstr),
			RA = parser:extract_a(DestInstr),
			case SB of
				0 ->
					RB = DB,
					Status = kill;
				true ->
					RB = Fun(DB, SB),
					Status = ok
			end;
		"ab" ->
			SA = parser:extract_a(SourceInstr),
			DB = parser:extract_b(DestInstr),
			RA = parser:extract_a(DestInstr),
			case SA of
				0 ->
					RB = DB,
					Status = kill;
				true ->
					RB = Fun(DB, SA),
					Status = ok
			end;
		"ba" ->
			SB = parser:extract_b(SourceInstr),
			DA = parser:extract_a(DestInstr),
			RB = parser:extract_a(DestInstr),
			case SB of
				0 ->
					RA = DA,
					Status = kill;
				true ->
					RA = Fun(DA, SB),
					Status = ok
			end;
		"f" ->
			SA = parser:extract_a(SourceInstr),
			SB = parser:extract_b(SourceInstr),
			DA = parser:extract_a(DestInstr),
			DB = parser:extract_b(DestInstr),
			case SA of
				0 ->
					RA = DA,
					RB = DB,
					Status = kill;
				true ->
					case SB of
						0 ->
							RA = DA,
							RB = DB,
							Status = kill;
						true ->
							RA = Fun(DA, SA),
							RB = Fun(DB, SB),
							Status = ok
					end
			end;
		"x" ->
			SA = parser:extract_a(SourceInstr),
			SB = parser:extract_b(SourceInstr),
			DA = parser:extract_a(DestInstr),
			DB = parser:extract_b(DestInstr),
			case SA of
				0 ->
					RA = DA,
					RB = DB,
					Status = kill;
				true ->
					case SB of
						0 ->
							RA = DA,
							RB = DB,
							Status = kill;
						true ->
							RA = Fun(DA, SB),
							RB = Fun(DB, SA),
							Status = ok
					end
			end;
		"i" ->
			SA = parser:extract_a(SourceInstr),
			SB = parser:extract_b(SourceInstr),
			DA = parser:extract_a(DestInstr),
			DB = parser:extract_b(DestInstr),
			case SA of
				0 ->
					RA = DA,
					RB = DB,
					Status = kill;
				true ->
					case SB of
						0 ->
							RA = DA,
							RB = DB,
							Status = kill;
						true ->
							RA = Fun(DA, SA),
							RB = Fun(DB, SB),
							Status = ok
					end
			end
	end,
	RCode = parser:extract_opcode(DestInstr),
	RMode = parser:extract_opmod(DestInstr),
	RAmod = parser:extract_amod(DestInstr),
	RBmod = parser:extract_bmod(DestInstr),
	{Status, server:create_cell(parser:create_instr(RCode, RMode, RAmod, RA, RBmod, RB), server:extract_warrior(Source))}.
	
execute_move(Source, Destination) ->
	SourceInstr = server:extract_instr(Source),
	DestInstr = server:extract_instr(Destination),
	InstrMod = parser:extract_opmod(SourceInstr),
	case InstrMod of
		"a" ->
			RCode = parser:extract_opcode(DestInstr),
			RMode = parser:extract_opmod(DestInstr),
			RAmod = parser:extract_amod(DestInstr),
			RA = parser:extract_a(SourceInstr),
			RBmod = parser:extract_bmod(DestInstr),
			RB = parser:extract_b(DestInstr);
		"b" ->
			RCode = parser:extract_opcode(DestInstr),
			RMode = parser:extract_opmod(DestInstr),
			RAmod = parser:extract_amod(DestInstr),
			RA = parser:extract_a(DestInstr),
			RBmod = parser:extract_bmod(DestInstr),
			RB = parser:extract_b(SourceInstr);
		"ab" ->
			RCode = parser:extract_opcode(DestInstr),
			RMode = parser:extract_opmod(DestInstr),
			RAmod = parser:extract_amod(DestInstr),
			RA = parser:extract_a(DestInstr),
			RBmod = parser:extract_bmod(DestInstr),
			RB = parser:extract_a(SourceInstr);
		"ba" ->
			RCode = parser:extract_opcode(DestInstr),
			RMode = parser:extract_opmod(DestInstr),
			RAmod = parser:extract_amod(DestInstr),
			RA = parser:extract_b(SourceInstr),
			RBmod = parser:extract_bmod(DestInstr),
			RB = parser:extract_b(DestInstr);
		"f" ->
			RCode = parser:extract_opcode(DestInstr),
			RMode = parser:extract_opmod(DestInstr),
			RAmod = parser:extract_amod(DestInstr),
			RA = parser:extract_a(SourceInstr),
			RBmod = parser:extract_bmod(DestInstr),
			RB = parser:extract_b(SourceInstr);
		"x" ->
			RCode = parser:extract_opcode(DestInstr),
			RMode = parser:extract_opmod(DestInstr),
			RAmod = parser:extract_amod(DestInstr),
			RA = parser:extract_b(SourceInstr),
			RBmod = parser:extract_bmod(DestInstr),
			RB = parser:extract_a(SourceInstr);
		"i" ->
			RCode = parser:extract_opcode(SourceInstr),
			RMode = parser:extract_opmod(SourceInstr),
			RAmod = parser:extract_amod(SourceInstr),
			RA = parser:extract_a(SourceInstr),
			RBmod = parser:extract_bmod(SourceInstr),
			RB = parser:extract_b(SourceInstr)
	end,
	{ok, server:create_cell(parser:create_instr(RCode, RMode, RAmod, RA, RBmod, RB), server:extract_warrior(Source))}.
	
execute_unconditional_jump(Core, N) ->
	Addr = get_source_address(Core, N),
	{ok, Addr}.
	
get_addresses(Core, N) ->
	{get_source_address(Core, N), get_destination_address(Core, N)}.
	
get_source_address(Core, N) ->
	Cell = server:nth_cell(N, Core),
	Instr = server:extract_instr(Cell),
	Field = parser:extract_a(Instr),
	Mod = parser:extract_amod(Instr),
	get_address(Core, N, Mod, Field).
	
get_destination_address(Core, N) ->
	Cell = server:nth_cell(N, Core),
	Instr = server:extract_instr(Cell),
	Field = parser:extract_b(Instr),
	Mod = parser:extract_bmod(Instr),
	get_address(Core, N, Mod, Field).
	
get_address(Core, N, Mod, Field) ->
	case Mod of
		'#' ->
			immediate_addressing(N);
		'$' ->
			direct_addressing(N, Field);
		'@' ->
			b_field_indirect_addressing(Core, N, Field);
		'*' ->
			a_field_indirect_addressing(Core, N, Field);
		'{' ->	% not implemented -- replaced with direct
			direct_addressing(N, Field);
		'}' ->	% not implemented -- replaced with direct
			direct_addressing(N, Field);
		'<' ->	% not implemented -- replaced with direct
			direct_addressing(N, Field);
		'>' ->	% not implemented -- replaced with direct
			direct_addressing(N, Field)
	end.
	
immediate_addressing(N) ->
	N.
	
direct_addressing(N, Field) ->
	N + Field.
	
b_field_indirect_addressing(Core, N, Field) ->
	Address = direct_addressing(N, Field),
	Cell = server:nth_cell(Address, Core),
	Instr = server:extract_instr(Cell),
	B = parser:extract_b(Instr),
	Address + B.
	
a_field_indirect_addressing(Core, N, Field) ->
	Address = direct_addressing(N, Field),
	Cell = server:nth_cell(Address, Core),
	Instr = server:extract_instr(Cell),
	A = parser:extract_a(Instr),
	Address + A.