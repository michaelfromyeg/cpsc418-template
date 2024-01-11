%% =====================================================================
%% @copyright 2011 Mark R. Greenstreet
%% @author Mark R. Greenstreet <mrg@cs.ubc.ca>
%% @end
%% =====================================================================
%%
%% @doc wlog - Add event logging to some functions from the
%%   <code>workers</code> module.  These functions behave like
%%   their counterparts from <code>workers</code>.  However if
%%   <code>workers:f(...)</code> returns <code>V</code>, then
%%   <code>wlog:f(...)</code> returns <code>{V, Log}</code> where
%%   <code>Log</code> is an event-log (see module <code>time_it</code>)
%%   of "meaningful" events in the the execution of <code>f</code>.
%%   Generally, this refers to events involving inter-process interactions.
-module wlog.
-export [reduce/3, reduce/4, retrieve/2].

%% @spec reduce(W, Leaf, Combine, Root) -> {term2(), event_log()}
%%    W = worker_pool()
%%    Leaf = fun((ProcState::worker_state) -> term1())
%%    Combine = fun((Left::term1(), Right::term1()) -> term1())
%%    Root = fun((term1()) -> term2())
%% @doc Like <code>workers:reduce(W, Leaf, Combine, Root)</code>.
%%   If <code>workers:reduce(W, Leaf, Combine, Root)</code> returns
%%   <code>V</code>, then we return <code>{V, Log}</code>, where
%%   <code>Log</code> records the times of interactions between the
%%   various processes involved in the reduce operation.
reduce(W, Leaf, Combine, Root) ->
  L1 = time_it:log("start"),
  MyPid = self(),
  NW = length(W),
  W1 = hd(W),
  L2 = time_it:log(L1,
  	"sending reduce request to worker pool, root=~w", [W1]),
  W1 ! fun(S) -> reduce(W, NW, MyPid, S, Leaf, Combine) end,
  L3 = time_it:log(L2, "waiting to receive response from ~w", [W1]),
  V = receive
    {W1, reduce, {V0, L4}} -> Root(V0)
    after workers:default_debug_timeout() ->
      misc:msg_dump([io_lib:format("{~w, reduce, V}", [W1])]),
      L4 = [],
      failed
  end,
  {V, time_it:log([L3 | L4], "result received, V = ~w from ~w", [V, W1])}.


%% @spec reduce(W, Leaf, Combine) -> {term1(), event_log()}
%%   W = worker_pool(),
%%   Leaf = fun((ProcState::worker_state()) -> term1()),
%%   Combine = fun((Left::term1(), Right::term1())->term1())
%% @doc Equivalent to <code>reduce(W, Leaf, Combine, IdentityFn)</code>,
%%   where <code>IdentityFn</code> is the identity function.
reduce(W, Leaf, Combine) ->  % default for Root is the identity function
  reduce(W, Leaf, Combine, fun(V) -> V end).

reduce(W, NW, ParentPid, S, Leaf, Combine) when (NW > 1) ->
  MyPid = hd(W),
  L1 = time_it:log("reduce(NW=~w, ParentPid=~w), start", [NW, ParentPid]), 
  {V, L2} = reduce2(W, NW, MyPid, S, Leaf, Combine),
  L3 = time_it:log([L1 | L2],
  	"reduce(NW=~w), sending ~w to ~w",
  	[NW, V, ParentPid]), 
  ParentPid ! {MyPid, reduce, {V, L3}},
  S;

reduce(W, 1, ParentPid, S, Leaf, _Combine) ->
  L1 = time_it:log(
  	"reduce(NW=1, ParentPid=~w), start",
  	[ParentPid]), 
  V = Leaf(S),
  L2 = time_it:log(L1,
  	"reduce(NW=1), sending ~w to ~w",
  	[V, ParentPid]), 
  ParentPid ! {hd(W), reduce, {V, L2} },
  S.

reduce2(W, NW, MyPid, S, Leaf, Combine) when (NW > 1) ->
  NW2 = NW div 2,
  {W1, W2} = lists:split(NW2, W),
  H2 = hd(W2),
  L1 = time_it:log("reduce2(NW=~w), sending request to ~w", [NW, H2]),
  H2 ! fun(S2) -> reduce(W2, NW-NW2, MyPid, S2, Leaf, Combine) end,
  {V1, L2} = reduce2(W1, NW2, MyPid, S, Leaf, Combine),
  L3 = time_it:log([L1 | L2],
      "reduce2(NW=~w), waiting for result from ~w", [NW, H2]),
  V2 = receive
    {H2, reduce, {V2a, L4}} -> V2a
    after workers:default_debug_timeout() ->
      misc:msg_dump([io_lib:format("{~w, reduce, V}", [H2])]),
      L4 = [],
      failed
  end,
  L5 = time_it:log([L4 | L3],
  	"reduce2(NW=~w), received ~w from ~w", [NW, V2, H2]),
  VV = if
    (V1 == failed) or (V2 == failed) -> failed;
    true -> Combine(V1, V2)
  end,
  {VV, L5};
reduce2(_W, 1, _MyPid, S, Leaf, _Combine) -> {Leaf(S), []}.


%% @spec retrieve(W, Fun) -> { Values, Log }
%%    W = worker_pool(),
%%    Fun = fun((ProcState::worker_state()) -> term()),
%%    Values = [ term() ],
%%    Log = event_log()
%% @doc Each worker evaluates <code>Fun</code>, and the results
%%   are assembled into the list <code>Values</code>.  The times
%%   of key events are recorded in <code>Log</code>.
%% @todo The <code>retrieve</code> function in <code>workers</code>
%%   supports many more variations than this one does.  Someday, I
%%   may provide the same generality here.
retrieve(W, Fun) ->
  Log0 = time_it:log("start sending retrieve requests"),
  Log1 = retrieve1(W, Fun, 0, self(), Log0),
  retrieve2(W, 0, [], Log1).

retrieve1([], _Fun, _Pos, _MyPid, Log) -> Log;
retrieve1([W1 | WT], Fun, Pos, MyPid, Log0) ->
  W1 ! fun(ProcState) ->
    MyPid ! {retrieve, Pos, Fun(ProcState)},
    ProcState
  end,
  retrieve1(WT, Fun, Pos+1, MyPid,
    time_it:log(Log0, "after sending retrieve request to ~w", [W1])).

retrieve2([], _Pos, V, Log) -> {V, Log};
retrieve2([W1 | WT], Pos, V, Log0) ->
  VV = receive
    {retrieve, Pos, X} -> X
  end,
  retrieve2(WT, Pos+1, [VV | V],
    time_it:log(Log0, "after receiving ~w from ~w", [VV, W1])).
