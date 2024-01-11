-module(doc).
-export([doc/0]).

%% @spec doc() -> ok
%% @doc Generate edoc documentation for the erlang modules in this directory.
doc() ->
  Files = ["misc.erl", "stat.erl", "time_it.erl", "wlog.erl", "workers.erl",
  	"wtree.erl"],
  Options  = [ {dir, "doc"}, {packages, false} ],
  edoc:run(Files, Options).
