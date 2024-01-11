%% =====================================================================
%% @copyright 2016 Mark R. Greenstreet
%% @author Mark R. Greenstreet <mrg@cs.ubc.ca>
%% @end
%% =====================================================================
%%
%% @doc wtree - Functions to support trees of processes.
%%   We arrange the processes of a worker pool to a (nearly) balanced
%%   binary tree.  We add entries to the process state list for each
%%   worker so that it knows its parent and its children.  We then
%%   provide functions <code>broadcast</code>, <code>reduce</code>
%%   and <code>scan</code> to operate on such trees.
%%   <p>
%%   The <code>receive</code> operations in <code>reduce</code> and
%%   <code>scan</code> include a time-out that by default is set to
%%   <code>'infinity'</code> (i.e., they'll hang forever if the expected
%%   message is never received).  This value can be changed for all
%%   workers in a worker_pool by the <code>set_debug_timout</code>
%%   function.
%%   </p>
%% @end

-module wtree.  % A tree of workers

-export [alive/1, broadcast/2, broadcast/3, children/1, create/1, create/0, init/1,
         nworkers/1, reap/1, reduce/3, reduce/4, scan/5, barrier/1,
	 set_debug_timeout/2, get_debug_timeout/1, default_debug_timeout/0,
	 put/2, put/3, get/2, get/3, rlist/3, rlist/4, random/2,
	 update/4, update/3, retrieve/2,
	 test/0, test_reduce/0, test_scan/0].

%% @spec create(N::integer()) -> worker_pool()
%% @doc Create a worker-pool of <code>N</code> processes and
%%   initialize them to support the tree operations of this module.
create(N) -> init(workers:create(N)).

%% @spec create() -> worker_pool()
%% @doc Create a worker-pool with the default number of processes and
%%   initialize them to support the tree operations of this module.
create()  -> init(workers:create()).

%% @spec init(W::worker_pool) -> worker_pool()
%% @doc Initialize the process state of the workers of <code>W</code>
%%   to support the tree operations of this module.
init([]) -> [];
init(W) when is_list(W) ->
  init2(hd(W), init1(W, length(W))),
  W.

init1([_Pid], 1) -> [];
init1(W, NW) ->
  NLeft = (NW+1) div 2,  % round high
  {W0, W1} = lists:split(NLeft, W),
  init2(hd(W1), init1(W1, NW-NLeft)),
  [ {NLeft, hd(W1)} | init1(W0, NLeft)].

init2(W, Children) ->
  W ! { fun(ProcState) ->
	  workers:put(ProcState,'$cs418:wtree:children$', Children)
	end,
	'wtree:init'
      }.


%% @equiv workers:alive
alive(W) -> workers:alive(W).

%% @equiv workers:reap(W)
reap(W) -> workers:reap(W).

%% @equiv workers:nworkers(W)
nworkers(W) -> workers:nworkers(W).

%% @spec children(ProcState::worker_state()) -> [{integer(), pid()}]
%% @doc Return a list of the children of this process.
%%   The ``tree'' 
children(ProcState) ->
  case workers:get(ProcState, '$cs418:wtree:children$', undefined) of
    [] -> [];
    ChildList = [{N_SubTree, CPid} | _Tl]
        when is_integer(N_SubTree), is_pid(CPid) ->
      ChildList;
    _ -> fail
  end.

%% @spec broadcast(W, Task, Args) -> ok
%% @doc Invoke <code>Task</code> on all workers.
%% <ul>
%%   <li><code>W</code> is a <code>worker_pool</code>.</li>
%%   <li><code>Task</code> is a function.  If
%%     <ul>
%%       <li><code>Task</code> has an arity of two, then it is invoked as
%%         <dl><dd><code>Task(ProcState, Arg)</code></dd></dl>
%%         where
%%         <ul>
%%           <li><code>ProcState</code> is the current state of the
%%		worker process, and</li>
%%           <li><code>Arg</code> is the <code>N</code><sup>th</sup> element
%%	        of <code>Args</code> when <code>Task></code> is invoked for
%%	  	the <code>N</code><sup>th</sup> worker of <code>W</code>.</li>
%%         </ul>
%%       </li>
%%       <li><code>Task</code> has an arity of three, then it is invoked as
%%         <dl><dd><code>Task(ProcState, Arg, N)</code></dd></dl>
%%	   where <code>ProcState</code>, <code>Arg</code>, and <code>N</code>
%%	   are as defined above.
%%       </li>
%%     </ul>
%%     The return value of <code>Task</code> becomes the new state of
%%     the process.
%%   </li>
%% </ul>
%% @todo Support the full functionality of <code>workers:broadcast</code>.

broadcast(W, Task, Args) when is_list(W), is_function(Task) ->
  case length(Args) == length(W) of
    true when W == [] -> ok;
    true ->
      hd(W) ! { fun(ProcState) -> bcast(ProcState, Task, Args, 1) end, 'wtree:broadcast' },
      ok;
    false -> erlang:error(io_lib:format(
      "wtree:broadcast -- ~s: ~w vs. ~w~n",
      [ "length(Args) not equal to number of workers",
      length(Args), length(W)]))
  end.

broadcast(W, Task) when is_list(W), is_function(Task, 1) ->
  broadcast(W, Task, lists:seq(1, length(W))).

bcast(ProcState, Task, Args, Index) ->
 bcast(ProcState, Task, Args, Index,
    element(2, lists:keyfind('$cs418:wtree:children$', 1, ProcState))).

bcast(ProcState, Task, Args, Index, [ {NLeft, RootRight} | ChildTail]) ->
  {A0, A1} = lists:split(NLeft, Args),
  RootRight ! { fun(PS) -> bcast(PS, Task, A1, Index+NLeft) end, 'wtree:broadcast' },
  bcast(ProcState, Task, A0, Index, ChildTail);

bcast(ProcState, Task, Args, Index, []) ->
  try
    if
      is_function(Task, 1) -> Task(ProcState);
      is_function(Task, 2) -> Task(ProcState, hd(Args));
      is_function(Task, 3) -> Task(ProcState, hd(Args), Index)
    end
  catch
    What:Reason ->
      io:format("~w in worker ~w: ~w~n", [What, self(), Reason]),
      io:format("Trying to recover~n"),
      ProcState
  end.

%% @spec reduce(W, Leaf, Combine, Root) -> term2()
%%    W = worker_pool(),
%%    Leaf = fun((ProcState::worker_state) -> term1()),
%%    Combine = fun((Left::term1(), Right::term1()) -> term1()),
%%    Root = fun((term1()) -> term2())
%% @doc A generalized reduce operation.
%%   The <code>Leaf()</code> function is applied in each worker.
%%   The results of these are combined, using a tree,
%%   using <code>Combine</code>.
%%   The <code>Root</code> function is applied to the final result
%%   from the combine tree to produce the result of this function.
%%   <br/>
%%   <b>Note:</b> The workers are ordered.  In particular, if one were
%%   to invoke <code>update(W, 'WID', lists:seq(1:Nworkers)</code>
%%   then all of the workers contributing to the <code>Left</code>
%%   argument will have <code>'WID'</code> values less than those
%%   contributing to the <code>Right</code>.  This interface says
%%   nothing about whether or not the trees are balanced.  This means
%%   that to get deterministic results, <code>Combine</code> should
%%   be an <a href="http://en.wikipedia.org/wiki/Associative_property">associative</a>
%%   function.
%% @todo Add an optional <code>Args</code> parameter so that
%%   <code>Leaf</code> can be an arity-2 function that is called with
%%   the worker process state and the element of <code>Args</code> for
%%   its process.
reduce(W = [W0 | _], Leaf, Combine, Root) ->
  V = case process_info(W0) of
    undefined -> {fail, [W0, "failure in reduce", assertion_failure, "root process is dead", []]};
    _ -> reduce_dispatch(W0, {Leaf, Combine}, true),
      case reduce_receive(W0, 100000) of
	{ok, TimeOut} -> reduce_receive(W0, TimeOut);
	F = {fail,_} -> F
      end
  end,
  check_for_fail(W, V, Root);
reduce([], _L, _C, _R) -> ok.  % empty worker pool, nothing to do

%% @spec reduce(W, Leaf, Combine) -> term()
%% @equiv reduce(W, Leaf, Combine, fun(X) -> X end)
reduce(W, Leaf, Combine) -> reduce(W, Leaf, Combine, fun(X) -> X end).

check_for_fail(W, {ok, V}, Root) ->
  case try_it(Root, [V], "Root", "reduce") of
    {ok, RootValue} -> RootValue;
    Fail={fail, _} -> check_for_fail(W, Fail, Root)
  end;
check_for_fail(W, {fail, F}, _) ->
  {F2, N_fail} = label_failures(W, F),
  Msg = if (N_fail == 1) -> "failure in worker process";
	   (N_fail > 0)  -> "failures in worker processes"
	end,
  throw({fail, Msg, F2}).
check_for_fail(W, V) -> check_for_fail(W, V, fun(X) -> X end).

label_failures(W, [Left, Right]) ->
  {Left2, N_left} = label_failures(W, Left),
  {Right2, N_right} = label_failures(W, Right),
  {[Left2, Right2], N_left + N_right};

label_failures(W, [Pid, Msg, Kind, Reason, Trace]) ->
  ProcName = case Pid == self() of
    true  -> "top-level";
    false -> lists:flatten(io_lib:format("worker ~p", [workerName(W, Pid)]))
  end,
  { [ Pid, ProcName, Msg, Kind, Reason,
      lists:takewhile(fun({Module,_,_,_}) -> Module /= wtree end, Trace)
    ],
    1
  }.

workerName(PidList, Pid) -> workerName(PidList, Pid, 0).
workerName([], Pid, _) -> Pid;
workerName([Pid | _], Pid, N) -> N;
workerName([_ | PidTail], Pid, N) -> workerName(PidTail, Pid, N+1).

reduce_work(ProcState, [{_, RootRight} | CT], LC = {_, Combine}) ->
  reduce_dispatch(RootRight, LC),
  Left = reduce_work(ProcState, CT, LC),
  Right = reduce_receive(RootRight, ProcState),
  combine(Combine, Left, Right, "reduce");
reduce_work(ProcState, [], {Leaf, _}) ->
  try_it(Leaf, [ProcState], "Leaf", "reduce").
  
reduce_dispatch(CPid, LC, RootFlag) ->
  PPid = self(),
  CPid ! { fun(PS) ->
      case RootFlag of
        true -> reduce_send(PPid, {ok, get_debug_timeout(PS)});
	_ -> ok
      end,
      V = case children(PS) of
	fail -> failure("failure in reduce", assertion_failure, "W is not a wtree");
	Kids -> reduce_work(PS, Kids, LC)
      end,
      reduce_send(PPid, V),
      PS
    end,
    {'wtree:reduce', 'dispatch'}
  }.
reduce_dispatch(CPid, LC) -> reduce_dispatch(CPid, LC, false).

reduce_send(PPid, V = {ok, _}) ->
  PPid ! {'$cs418:wtree:reduce$', self(), V};
reduce_send(PPid, V={fail, _}) ->
  PPid ! {'$cs418:wtree:reduce$', self(), V};
reduce_send(PPid, fail) ->
  reduce_send(PPid, {fail, fail}).

reduce_receive(CPid, TimeOut) when is_integer(TimeOut) ->
  receive
    {'$cs418:wtree:reduce$', CPid, V} -> V;
    exit -> exit(ok)
    after TimeOut ->
      {fail, [self(), error, time_out,
        misc:msg_dump("wtree:reduce", [io_lib:format("{'$cs418:wtree:reduce$', ~w, V}", [CPid])]),
	[]]}
  end;
reduce_receive(CPid, PS) -> reduce_receive(CPid, get_debug_timeout(PS)).

combine(CombineFun, Left, Right, What) ->
  case {Left, Right} of
    {{ok, LeftValue}, {ok, RightValue}} ->
      try_it(CombineFun, [LeftValue, RightValue], "Combine", What);
    {{fail, _}, {ok, _}} -> Left;
    {{ok, _}, {fail, _}} -> Right;
    {{fail, LeftFail}, {fail, RightFail}} -> {fail, [LeftFail, RightFail]};
    _ -> failure("internal error in " ++ What, error, badmatch)
  end.

try_it(Fun, Args, Who, What) ->
  try {ok, apply(Fun, Args)}
  catch
    Class:Reason:StackTrace ->
      failure("error in " ++ Who ++ " function for wtree:" ++ What,
              Class, Reason, StackTrace)
  end.

failure(ErrMsg, Class, Reason, StackTrace) ->
  {fail, [self(), ErrMsg, Class, Reason, StackTrace]}.

failure(ErrMsg, Class, Reason) ->
  {fail, [self(), ErrMsg, Class, Reason, []]}.

fail_now({fail, F}) ->
  io:format("~p~n", [element(1, label_failures([], F))]).

%% @spec scan(W, Leaf1, Leaf2, Combine, Acc0) -> term1()
%%    W = worker_pool(),
%%    Leaf1 = fun((ProcState::worker_state) -> term1()),
%%    Leaf2 = fun((ProcState::worker_state, AccIn::term1()) -> worker_state()),
%%    Combine = fun((Left::term1(), Right::term1()) -> term1()),
%%    Acc0 = (term1() | fun((term(1)) -> {term2(), term1()}))
%% @doc A generalized scan operation.
%%   The <code>Leaf1()</code> function is applied in each worker process.
%%   The results of these are combined, using a tree,
%%   using <code>Combine</code>.
%%   <br/>
%%   Let <code>ReduceValue</code> denote the result of applying the <code>Combine</code>
%%   function at the root.  
%%   If <code>Acc0</code> is not a function of arity 1, the value of <code>Acc0</code>
%    is used as the initial accumulator for the left subtree, and
%%   <code>scan(...)</code> returns <code>ReduceValue</code>.
%%   If <code>Acc0</code> is a function of arity-1, let
%%   {ReturnValue, RootValue} = Acc0(ReduceValue).  In this case, <code>RootValue</code>
%    is used as the initial accumulator for the left subtree, and<br/>
%%   <code>scan(W, Leaf1, Leaf2, Combine, Acc0)</code><br/>
%%   returns <code>ReturnValue</code>.
%%   <br/>
%%   Results are propagated back down the tree.
%%   The <code>Leaf2()</code> function is applied in each
%%   worker process.  The <code>AccIn</code> argument is the result
%%   of the <code>Combine</code> for everything to the left of this
%%   node in the tree.  For the leftmost process, <code>Acc0</code>
%%   is used.  The return value of <code>Leaf2</code> becomes the
%%   state of the worker process.
scan(W = [W0 | _], Leaf1, Leaf2, Combine, Acc0) ->
  V = case process_info(W0) of
    undefined -> {fail, [W0, "failure in scan", assertion_failure, "root process is dead", []]};
    _ ->
      scan_dispatch(W0, {Leaf1, Leaf2, Combine}, true),
      case scan_receive(W0, 100000) of
	{ok, TimeOut} ->
	  case is_function(Acc0, 1) of
	    true ->
	      case scan_receive(W0, TimeOut) of
	        {ok, ReduceValue} ->
	          {ReturnValue, RootValue} =
		    case try_it(Acc0, [ReduceValue], "Root", "scan") of
		      {ok, TopV} -> TopV;
		      Fail = {fail,_} -> {Fail, Fail}
		    end,
		  scan_send(W0, {ok, RootValue}),
		  {ok, ReturnValue};
	        F = {fail, _} ->
		  scan_send(W0, F),
		  F
	      end;
	    false ->
	      scan_send(W0, {ok, Acc0}),
	      scan_receive(W0, TimeOut)
	  end;
	F = {fail, _} -> F
      end
  end,
  check_for_fail(W, V);
scan([], _L1, _L2, _C, _A0) -> ok.  % empty worker pool, nothing to do

scan_dispatch(CPid, LLC, RootFlag) ->
  PPid = self(),
  CPid ! {
    fun(PS) ->
      case RootFlag of
        true -> scan_send(PPid, {ok, get_debug_timeout(PS)});
	_ -> ok
      end,
      {Kids, Scan1} = case children(PS) of
	fail -> {undefined, failure("failure in scan", assertion_failure, "W is not a wtree")};
	K -> {K, scan_work1(PS, K, LLC)}
      end,
      scan_send(PPid, case Scan1 of
	{ok, [V1 | _]} ->
	  {ok, V1};
	{fail, _} -> Scan1
      end),
      case Kids of
	undefined -> PS;  % failure, leave worker state unchanged
	_ ->
	  scan_work2(PS, Kids, LLC, scan_receive(PPid, PS), ok_tl(Scan1))
      end
    end,
    {'wtree:scan', dispatch}
  }.
scan_dispatch(CPid, LLC) -> scan_dispatch(CPid, LLC, false).

scan_work1(ProcState, [{_, RootRight} | CT], LLC) ->
  scan_dispatch(RootRight, LLC),
  Left  = scan_work1(ProcState, CT, LLC),
  CombineFun = element(3, LLC),
  Right = scan_receive(RootRight, ProcState),
  ok_cons(combine(CombineFun, ok_hd(Left), Right, "scan"), Left);
scan_work1(ProcState, [], LLC) ->
  case try_it(element(1, LLC), [ProcState], "Leaf1", "scan") of
    {ok, V} -> {ok, [V]};
    F={fail, _} -> F
  end.

ok_hd({ok, [Hd | _]}) -> {ok, Hd};
ok_hd(F = {fail, _}) -> F.

ok_tl({ok, [_ | Tl]}) -> {ok, Tl};
ok_tl(F = {fail, _}) -> F.

ok_cons({ok, Hd}, {ok, Tl}) -> {ok, [Hd | Tl]};
ok_cons(F={fail, _}, _) -> F.

scan_work2(ProcState, [], LLC, {ok, AccIn}, _) ->
  Leaf2 = element(2, LLC),
  Result = case try_it(Leaf2, [ProcState, AccIn], "Leaf2", "scan") of
    {ok, ok}   -> {ok, ProcState};  % ok means 'keep old state'
    {ok, NewState} ->
      case workers:goodState(NewState) of
        true  -> {ok, NewState};
	false -> failure("Leaf2 function for scan returned invalid process state", assertion_failure, [])
      end;
    F = {fail, _} -> F
  end,
  case Result of
    {ok, State} -> State;
    Fail ->
      fail_now(Fail),
      ProcState
  end;
scan_work2(ProcState, Kids=[{_, RootRight} | CT], LLC, AccIn = {ok,_}, LL={ok,_}) ->
  Combine = element(3, LLC),
  case combine(Combine, AccIn, ok_hd(LL), "scan") of
    {ok, ToRight} ->
      scan_send(RootRight, {ok, ToRight}),
      scan_work2(ProcState, CT, LLC, AccIn, ok_tl(LL));
    Fail ->
      scan_work2_fail(Kids, Fail),
      fail_now(Fail),
      ProcState
  end;
scan_work2(ProcState, Kids, _LLC, AccInFail = {fail,_}, _LL) ->
  scan_work2_fail(Kids, AccInFail),
  ProcState;
scan_work2(ProcState, Kids, _LLC, _AccIn, LLFail = {fail,_}) ->
  scan_work2_fail(Kids, LLFail),
  ProcState.

scan_work2_fail([{_, RootRight} | CT], Fail) ->
  scan_send(RootRight, Fail),
  scan_work2_fail(CT, Fail);
scan_work2_fail([], _Fail) -> fail.

scan_send(Pid, V)   ->
  Pid ! {'$cs418:wtree:scan$', self(), V}.

scan_receive(Pid, TimeOut) when is_integer(TimeOut) ->
  receive
    {'$cs418:wtree:scan$', Pid, V} -> V;
    exit -> exit(ok)
    after TimeOut ->
      {fail, [self(), error, time_out,
        misc:msg_dump("wtree:scan", [io_lib:format("{'$cs418:wtree:scan$', ~w, V}", [Pid])]),
	[]]}
  end;
scan_receive(CPid, PS) -> scan_receive(CPid, get_debug_timeout(PS)).

%% @spec barrier(W) -> ok
%% @doc A barrier for worker-pool <code>W</code>.
barrier(W) ->
  reduce(W, fun(_) -> ok end, fun(_,_) -> ok end),
  ok.

%% @spec set_debug_timeout(W, T) -> ok
%% @equiv workers:set_debug_timeout(W, T)
set_debug_timeout(W, T) -> workers:set_debug_timeout(W, T).

%% @spec get_debug_timeout(ProcState) -> integer()
%% @equiv workers:get_debug_timeout(ProcState)
get_debug_timeout(ProcState) -> workers:get_debug_timeout(ProcState).

%% @spec default_debug_timeout() -> integer()
%% @equiv workers:get_debug_timeout()
default_debug_timeout() -> workers:default_debug_timeout().

%% @spec get(ProcState, Key, Default) -> term()
%% @equiv workers:get(ProcState, Key, Default)
get(ProcState, Key, Default) -> workers:get(ProcState, Key, Default).

%% @spec get(ProcState, Key) -> term()
%% @equiv workers:get(ProcState, Key)
get(ProcState, Key) -> workers:get(ProcState, Key).


%% @spec put(ProcState, Key, Value) -> NewProcState
%% @equiv workers:put(ProcState, Key, Value)
put(ProcState, Key, Value) -> workers:put(ProcState, Key, Value).

%% @spec put(ProcState, TupleList) -> NewProcState
%% @equiv workers:put(ProcState, TupleList)
put(ProcState, TupleList) -> workers:put(ProcState, TupleList).

%% @spec rlist(W, N, M, Key) -> ok
%% @equiv  workers:rlist(W, N, M, Key)
rlist(W, N, M, Key) ->
  broadcast(W,
	    fun(ProcState, NN) ->  workers:rlist2(NN, M, ProcState, Key) end,
	    [Hi-Lo || {Lo,Hi} <- misc:intervals(0, N, nworkers(W))]).

%% @spec rlist(W, N, Key) -> ok
%% @equiv workers:rlist(W, N, Key)
rlist(W, N, Key) -> rlist(W, N, 1.0, Key).

%% @spec random(M, ProcStat0) -> {RandNum, ProcState1}
%% @equiv workers:random(M, ProcState0)
random(M, ProcState0) -> workers:random(M, ProcState0).

%% @spec  update(W, Key, Fun, Args) -> ok
%% @equiv workers:update(W, Key, Fun, Args)

% Implementation note: update/4 and update/3 are just cut-and-paste from
% workers:update/4 and workers:update/3.  Because the call to broadcast/3
% in this module calls wtree:broadcast, we get the tree-version of
% communication and should be race-free wrt. other operations on wtree's.
% I'm not happy with cut-and-paste code.  Perhaps workers:update should
% have a version that takes an argument for the broadcast function?
update(W, Key, Fun, Args) ->
  F0 = if 
    is_function(Fun, 2) ->
      fun(ProcState, _ProcIndex, Arg) -> Fun(ProcState, Arg) end;
    is_function(Fun, 3) -> Fun
  end,
  F1 = fun(ProcState, ProcIndex, Arg) ->
    workers:put(ProcState, Key, F0(ProcState, ProcIndex, Arg))
  end,
  broadcast(W, F1, Args).


%% @spec  update(W, Key, X) -> ok
%% @equiv workers:update(W, Key, X)
update(W, Key, X) ->
    F = if
    is_function(X, 0) -> fun(_ProcState, _N, _A) -> X() end;
    is_function(X, 1) -> fun(ProcState, _N, _A) -> X(ProcState) end;
    is_function(X, 2) -> fun(ProcState, N, _A) -> X(ProcState, N) end;
    is_list(X) -> fun(_ProcState, _N, A) -> A end
  end,
  update(W, Key, F, if is_list(X) -> X; true -> W end).

%% @spec retrieve(W::worker_pool(), X) -> Values::List
%% @doc Return a list of the values from each process of <code>W</code>.
%% <ul>
%%   <li>If <code>X</code> is a function of one argument, then the value for worker process <code>W_I</code> is
%%     <code>X(ProcState_I)</code> where <code>ProcState_I</code> is the per-worker process state
%%     for worker <code>W_I</code>.</li>
%%   <li>Otherwise, the value for worker proces <code>W_I</code> is <code>wtree:get(ProcState_I, X)</code>.</li>
%% </ul>
retrieve(W, F) when is_function(F, 1) ->
  [V || {V} <- lists:flatten(reduce(W,
				    fun(ProcState) -> [{F(ProcState)}] end,
				    fun(Left, Right) -> [Left, Right] end))];
retrieve(W, Key) ->
  retrieve(W, fun(ProcState) -> wtree:get(ProcState, Key) end).


%% @hidden
%% TODO: rewrite these using EUnit.
test_reduce() ->
  NWorkers = 12,
  Block = 5,
  W = create(NWorkers),
  set_debug_timeout(W, 1000),
  workers:update(W, seq,
    fun(_PS, I) -> lists:seq(Block*(I-1), (Block*I)-1) end),
  M = NWorkers*Block - 1,
  V0 = M*(M+1) div 2,
  V1 = reduce(W,
    fun(PS) -> lists:sum(workers:get(PS, seq)) end,
    fun(L, R) -> L+R end),
  V2 = reduce(W,
    fun(PS) -> lists:sum(workers:get(PS, seq)) end,
    fun(L, R) -> L+R end),
  case {V1, V2} of
    {V0, V0} ->  ok;
    {V0, _} -> 
      io:format("failed: got sum_{k=0}^~w k = ~w, should be ~w~n",
		[M, V2, V0]),
      failed;
    {_, _} -> 
      io:format("failed: got sum_{k=0}^~w k = ~w, should be ~w~n",
    	        [M, V1, V0]),
      failed
  end.
    

%% @hidden
test_scan() ->
  W = create(12),
  workers:update(W, seq, fun(_PS, I) -> lists:seq(5*(I-1), (5*I)-1) end),
  scan(W,
    fun(PS) -> lists:sum(workers:get(PS, seq)) end,
    fun(PS, Acc0) ->
      workers:put(PS, cumseq, misc:cumsum(Acc0, workers:get(PS, seq)))
    end,
    fun(L, R) -> L+R end,
    0),
  V1 = lists:flatten(workers:retrieve(W, cumseq)),
  workers:update(W, char, fun(_PS, I) -> 96+I end),
  scan(W,
    fun(PS) -> [workers:get(PS, char)] end,
    fun(PS, Acc0) ->
      workers:put(PS, str, Acc0 ++ [workers:get(PS, char)])
    end,
    fun(L, R) -> L ++ R end,
    []),
  V2 = workers:retrieve(W, str),
  {V1, V2}.

%% @hidden
test() -> test_scan().
