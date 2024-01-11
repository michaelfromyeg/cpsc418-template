-module wtree2.  % A tree of workers

-export [broadcast/3, children/1, create/1, create/0, init/1,
         nworkers/1, parent/1, reap/1, reduce/3, reduce/4,
	 set_debug_timeout/2, test/0].


create(N) -> init(workers:create(N)).

create()  -> init(workers:create()).

init([]) -> [];
init(W) ->
  init(W, self(), length(W)),
  workers:update(W, {'$wtree$', debug_timeout}, fun(_) -> infinity end),
  W.

init(W, Parent, NW) -> init2(hd(W), Parent, init(W, NW)).

init([_Pid], 1) -> [];
init(W = [Pid | _], NW) ->
  NLeft = (NW+1) div 2,  % round high
  {W0, W1} = lists:split(NLeft, W),
  init(W1, Pid, NW-NLeft),
  [ {NLeft, hd(W1)} | init(W0, NLeft)].

init2(W, Parent, Children) ->
  W ! fun(ProcState) ->
    workers:put(ProcState,
      [ { {'$wtree$', parent}, Parent},
        { {'$wtree$', children}, Children}
      ]
    )
    end.


reap(W) -> workers:reap(W).

nworkers(W) -> workers:nworkers(W).

parent(ProcState) -> workers:get(ProcState, {'$wtree$', parent}).

children(ProcState) -> workers:get(ProcState, {'$wtree$', children}).


broadcast(W, Task, Args) ->
  case length(Args) == length(W) of
    true when W == [] -> ok;
    true ->
      hd(W) ! fun(ProcState) -> bcast(ProcState, Task, Args, 1) end,
      ok;
    false -> erlang:error(io_lib:format(
      "wtree:broadcast -- ~s: ~w vs. ~w~n",
      [ "length(Args) not equal to number of workers",
      length(Args), length(W)]))
  end.

bcast(ProcState, Task, Args, Index) ->
  bcast(ProcState, Task, Args, Index,
    element(2, lists:keyfind({'$wtree$', children}, 1, ProcState))).

bcast(ProcState, Task, Args, Index, [ {NLeft, RootRight} | ChildTail]) ->
  {A0, A1} = lists:split(NLeft, Args),
  RootRight ! fun(PS) -> bcast(PS, Task, A1, Index+NLeft) end,
  bcast(ProcState, Task, A0, Index, ChildTail);

bcast(ProcState, Task, Args, Index, []) ->
  if
    is_function(Task, 2) -> Task(ProcState, hd(Args));
    is_function(Task, 3) -> Task(ProcState, hd(Args), Index)
  end.


reduce([W0 | _], Leaf, Combine, Root) ->
  io:format("~w: reduce([~w| _], ~w, ~w)~n", [W0, Leaf, Combine, Root]),
  reduce_dispatch(W0, {Leaf, Combine}),
  Root(reduce_receive(W0, []));
reduce([], _L, _C, _R) -> ok.  % empty worker pool, nothing to do

reduce(W, Leaf, Combine) -> reduce(W, Leaf, Combine, fun(X) -> X end).

reduce_work(ProcState, CC = [{_, RootRight} | CT], LC = {_, Combine}) ->
  io:format("~w: reduce_work(PS, ~w, ~w)~n", [self(), CC, LC]),
  reduce_dispatch(RootRight, LC),
  Left = reduce_work(ProcState, CT, LC),
  Right = reduce_receive(RootRight, ProcState),
  Combine(Left, Right);
reduce_work(ProcState, [], LC = {Leaf, _}) ->
  io:format("~w: reduce_work(PS, [], ~w)~n", [self(), LC]),
  Leaf(ProcState).
  
reduce_dispatch(CPid, LC) ->
  io:format("~w: reduce_dispatch(~w, ~w)~n", [self(), CPid, LC]),
  CPid ! fun(PS) ->
    V = reduce_work(PS, children(PS), LC),
    VV = {'$wtree$', reduce, self(), V },
    io:format("~w: sending ~w to ~w~n", [self(), VV, parent(PS)]),
    parent(PS) ! {'$wtree$', reduce, self(), V }
  end.

reduce_receive(CPid, PS) ->
  receive
    {'$wtree$', reduce, CPid, V} -> V
    after debug_timeout(PS) ->
      misc:msg_dump("wtree:reduce",
        [io_lib:format("{'$wtree$', reduce, ~w, V}", [CPid])])
  end.


debug_timeout(PS) ->
  workers:get(PS, {'$wtree$', debug_timeout}, fun() -> infinity end).

set_debug_timeout(W, T) ->
  workers:update(W, {'$wtree$', debug_timeout} , fun() -> T end),
  ok.


test() ->
  W = create(4),
  io:format("~w: W = ~w~n", [self(), W]),
  set_debug_timeout(W, 1000),
  workers:update(W, seq, fun(_PS, I) -> lists:seq(5*(I-1), (5*I)-1) end),
  reduce(W,
    fun(PS) -> lists:sum(workers:get(PS, seq)) end,
    fun(L, R) -> L+R end).
