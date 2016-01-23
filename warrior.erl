-module(warrior).
-compile([export_all]).

loop(Location) ->
	receive
		{Pid, execute, Core} ->
			io:format("odebrane ~w~n", [Pid]),
			Pid ! ok;
			
		{Pid, kill} ->
			Pid ! dead,
			dead
	end.