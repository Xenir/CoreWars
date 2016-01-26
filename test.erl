-module(test).
-compile([export_all]).

run() ->
	ServerPid = server:init(),
	server:load_warrior(ServerPid, "dwarftest"),
	server:load_warrior(ServerPid, "imptest"),
	Steps = 100,
	ServerPid ! {self(), start, Steps}.