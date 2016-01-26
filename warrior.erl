-module(warrior).
-compile([export_all]).

loop(Location) ->
	receive
		{Pid, execute, Core} ->
			io:format("odebrane ~w~n", [Pid]),
			{Status, _, NewCore, NewLocation} = mech:execute(Core, Location),
			case Status of
				ok ->
					Pid ! {self(), ok, NewCore},
					loop(NewLocation);
				kill ->
					Pid ! {self(), dead, NewCore},
					dead
			end;

		{Pid, kill} ->
			Pid ! {self(), dead},
			dead
	end.