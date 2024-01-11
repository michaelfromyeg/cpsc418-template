%% =====================================================================
%% @copyright 2011 Mark R. Greenstreet
%% @author Mark R. Greenstreet <mrg@cs.ubc.ca>
%% @end
%% =====================================================================
%%
%% @doc workers - create and manage a pool of worker processes.
%% Each worker process has a process state.  The process state is an
%% association list of tuples of the form <code>{Key, Value}</code>,
%% where <code>Key</code> and <code>Value</code> are arbitrary Erlang terms
%% When a process is created, this list is empty.
%%
%% Worker process wait to receive tasks.  The task is invoked on the
%% current state and returns a new state.
%%
%% @type worker_pool().  Abstract type for a worker pool.
%% @type worker_state().  Abstract type for a worker process state.
%%
%% @end

-module workers.
-export [create/1, create/0, default_n/0, alive/1, reap/1, nworkers/1].
-export [get/2, get/3, keys/1, put/2, put/3].
-export [broadcast/2, broadcast/3, retrieve/2, retrieve/3, retrieve/4, update/3, update/4].
-export [rlist/3, rlist/4, rlist2/4, random/2, random/3, seq/4, seq/5].
-export [initState/0, goodState/1].
-export [set_debug_timeout/2, get_debug_timeout/1, default_debug_timeout/0].

% ------------------------------------------------------------------------
% Basic processes for worker pools:
%   create(N) ->   a worker pool of N processes.
%   nworkers(W) -> the number of workers in pool W.
%   reap(W) ->     terminate the workers in pool W.
% ------------------------------------------------------------------------

% workerProc: a worker process
%   The state of a function is maintained in the parameter S.
%   The workerProc function waits to receive a function, Update.
%   Then, the process continues with S replaced by Update(S).
workerProc(ProcState) ->
  workerProc(receive
    TaskFun when is_function(TaskFun, 1) -> worker_do(ProcState, TaskFun, none);
    {TaskFun, From} when is_function(TaskFun, 1) -> worker_do(ProcState, TaskFun, From);
    {fail, _} -> workerProc(ProcState); % clean-out 'fail' messages.
    exit -> exit(normal)
  end).

worker_do(ProcState, TaskFun, From) ->
  try NewState = TaskFun(ProcState),
    case {NewState, goodState(NewState)} of
      {ok, _}   -> ProcState;  % ok means 'keep old state'
      {_, true} -> NewState;
      _ ->
	io:format("~p: Task function returned invalid process state -- ~p~n~s",
	  [ self(), NewState,
	    case From of
	      none -> "";
	      _ ->
		lists:flatten(io_lib:format("  Task received from ~p~n", [From]))
	    end
	  ]),
	ProcState
    end
  catch
    throw:Exception:Stack -> oops("Throw", Exception, Stack), ProcState;
    error:Reason:Stack    -> oops("Error", Reason, Stack),    ProcState
  end.

oops(What, Why, Stack) ->
  io:format("~s in worker ~p: ~p~n  stack trace:~n  ~p~n  Trying to recover.~n",
	    [What, self(), Why, Stack]).

% I'm assuming that the association list for a process (aka ProcState)
%   should never be empty.  That's because the initialization procedure
%   adds a few entries, and we have no mechanism for deleting bindings
%   (only overwriting them).
goodState(ok) -> true;
goodState(L) when is_list(L) ->
  lists:all(fun(X) -> is_tuple(X) andalso (size(X) == 2) end, L);
goodState(_) -> false.


% create the initial state (an association list) for a worker process
initState() ->
  % initialize the random number generator
  % using self() as a seed has two "nice" properties:
  %   First, each Erlang process gets it's own sequence of pseudo-random values.
  %   Second, if we run Erlang twice and give *exactly* the same sequence of commands,
  %     then we should get exactlythe same pseudo-random sequences.
  X = erlang:phash2(self()),
  State0 = rand:seed_s(exsplus, {X, X*(X+17), X*(X-42)*(X+18780101)}),
  [{ '$cs418:workers:randomState$', State0 }].

%% @spec create(N::integer()) -> worker_pool()
%% @doc  Spawn <code>N</code> worker processes.
create(0) -> [];
create(N) when is_integer(N), N > 0 ->
  [ spawn(fun() -> workerProc(initState()) end) | create(N-1)].

%% @spec create() -> worker_pool()
%% @equiv create(default_n())
create() -> create(default_n()).

%% @spec default_n() -> integer()
%% @doc  A default for the number of workers for a worker pool.
default_n() -> erlang:system_info(schedulers).

%% @spec nworkers(W::worker_pool()) -> integer()
%% @doc  Return the number of workers in <code>W</code>.
nworkers(W) -> length(W).

%% @spec alive(W::worker_pool()) -> true | false
%% @doc  Return true if all of the processes in worker_pool() are alive.
alive(W) -> lists:all(fun(WW) -> process_info(WW) /= undefined end, W).

%% @spec reap(W::worker_pool()) -> ok
%% @doc  Terminate the worker processes of <code>W</code>.
reap(W) -> [WW ! exit || WW <- W], ok.



% ------------------------------------------------------------------------
% Functions that workers use to access their process state
%   get(S, Key) -> the value associated with Key or 'undefined'.
%   put(S1, Key, Value) -> S2 updated to associate Value with Key
%   put(S1, TupleList) -> S2 updated to associate element(2, X)
%                                with element(1,X) for each X of TupleList. 
%   keys(S) -> a list of all of the Keys in the association list.
% ------------------------------------------------------------------------

%% @spec get(ProcState, Key, DefaultFn) -> term()
%%   ProcState = worker_state(),
%%   Key = term(),
%%   DefaultFn = (   undefined
%%                 | fun(() -> term())
%%                 | fun((ProcState::worker_state()) -> term())
%%                 | fun((ProcState::worker_state(), Key::worker_state()) -> term())
%%   )
%% @end
%% @doc Get that value associated with <code>Key</code> in process state <code>ProcState</code>.
%%   Parameters:
%%   <ul>
%%      <li> <code>ProcState</code>: The state of the worker process.
%%      </li>
%%      <li> <code>Key</code>: The key associated with the desired value.
%%      </li>
%%      <li> <code>DefaultFn</code>: If <code>S</code> associates no value with
%%        <code>Key</code> then <code>DefaultFn</code> is used to determine the
%%        result of <code>get(...)</code>.
%%      </li>
%%   </ul>
%%   Result:
%%     if there is a value associated with <code>Key</code> in <code>S</code>,
%%     then that value is returned.  Otherwise, the return value is determined
%%     by <code>Default</code>:
%%     <ul>
%%       <li> If <code>Default</code> is an atom, then <code>Default</code> is returned.
%%       </li>
%%       <li> If <code>Default</code> is a function with arity 0, then
%%          the return value is <code>Default()</code>.
%%       </li>
%%       <li> If <code>Default</code> is a function with arity 1, then
%%          the return value is <code>Default(S)</code>.
%%       </li>
%%       <li> If <code>Default</code> is a function with arity 3, then
%%          the return value is <code>Default(S, Key)</code>.</li>
%%       <li> If <code>Default</code> does not match any of these patterns,
%%          then an error is thrown.
%%       </li>
%%     </ul>
get(ProcState, Key, Default) ->
  case lists:keyfind(Key, 1, ProcState) of
    {Key, Value} -> Value;
    false ->
      if
        is_atom(Default) -> Default;
	is_function(Default, 0) -> Default();
	is_function(Default, 1) -> Default(ProcState);
	is_function(Default, 2) -> Default(ProcState, Key)
      end
  end.

%% @spec get(ProcState::worker_state(), Key::term()) -> term()
%% @doc Get that value associated with <code>Key</code> in process state <code>ProcState</code>.
%%   If there is a value associated with <code>Key</code> in <code>S</code>,
%%   then that value is returned.  Otherwise, an exception is thrown.
get(ProcState, Key) ->
  workers:get(ProcState, Key,
    fun() -> throw({'workers:get', {pid, self()}, {not_found, Key}}) end).


%% @spec put(ProcState::worker_state(), Key::term(), Value::term()) -> worker_state()
%% @doc Update state <code>S</code> to associate <code>Value</code> with
%%   <code>Key</code>.
put(ProcState, Key, Value) -> lists:keystore(Key, 1, ProcState, {Key, Value}).

%% @spec put(ProcState, TupleList) -> worker_state()
%%   ProcState = worker_state(),
%%   TupleList = [ { Key::term(), Value::term() } ]
%% @doc Update state <code>S</code> so that each <code>Value</code> is
%%   associated with its corresponding <code>Key</code>.
put(ProcState, []) -> ProcState;
put(ProcState, [{Key, Value} | Tail]) ->
  workers:put(workers:put(ProcState, Key, Value), Tail).

%% @spec keys(ProcState::worker_state()) -> [term()]
%% @doc return a list of all of the keys for association in <code>S</code>
keys(ProcState) -> [ Key || {Key, _Value} <- ProcState].



% ------------------------------------------------------------------------
% Functions that the root process uses to interact with worker processes
%   broadcast(W, Task, [Args]) ->  send the Task to each worker.
%   retrieve(W, Fun, [Args])   ->  evaluate Fun in each worker,
%                                  and collect the results
%   update(W, Key, Fun, [Args]) -> update the value associated with Key in
%                                  each process with the result of applying
%                                  Fun in that process.
% ------------------------------------------------------------------------

%% @spec broadcast(W::worker_pool(), Task) -> ok
%%    Task = (   fun((ProcState::worker_state()) -> worker_state())
%%             | fun((S::worker_state(), N::integer) -> worker_state())
%%    )
%% @doc Each worker process performs Task.
%% If
%% <ul>
%%   <li><code>Task</code> is a function with arity 1,
%%     then it is called with the current process state.
%%     The process state is updated to the return value of <code>Task</code>.
%%   </li>
%%   <li><code>Task</code> is a function with arity 2,
%%     then the first parameter is the current process state,
%%     and the second parameter is the index of the process.
%%     The process state is updated to the return value of <code>Task</code>.
%%     Process indices range from 1 to
%%     <code>{@link nworkers/1. nworkers}(W)</code>.
%%   </li>
%% </ul>
%% The value returned by <code>Task</code> becomes the new state
%% for the process.
broadcast(W, Task) when is_function(Task, 1) ->
  [ WW ! {Task, 'workers:broadcast'} || WW <- W ], ok;
broadcast(W, Task) when is_function(Task, 2) ->
  [    WW ! {fun(ProcState) -> Task(ProcState, ProcIndex) end, 'workers:broadcast'}
    || {WW, ProcIndex} <- lists:zip(W, lists:seq(1, length(W)))
  ], ok.

%% @spec broadcast(W::worker_pool(), Task, Args::List) -> ok
%%    Task = (   fun((ProcState::worker_state(), Arg) -> worker_state())
%%            | fun((ProcState::worker_state(), N::integer, Arg) -> worker_state())
%%    ),
%%    length(Args) = workers:nworkers(W)
%% @doc Each worker process performs <code>Task</code>, where <code>Task</code>
%% is called with a per-worker argument.
%% If
%% <ul>
%%   <li><code>Task</code> is a function with arity 2,
%%     then it is called with the current process state and
%%     <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>,
%%     where <code>N</code> is the index of the process in <code>W</code>.
%%     Process indices range from 1 to
%%     <code>{@link nworkers/1. nworkers}(W)</code>.
%%   </li>
%%   <li><code>Task</code> is a function with arity 3, then the first
%%     parameter is the current process state, the second parameter
%%     is the index of the process, and the third parameter is
%%     <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>.
%%   </li>
%% </ul>
%% The value returned by <code>Task</code> becomes the new state
%% for the process.
broadcast(W, Fun, Args) when is_function(Fun, 2) ->
  broadcast(W, fun(ProcState, _N, A) -> Fun(ProcState, A) end, Args);
broadcast(W, Fun, Args) when is_function(Fun, 3) ->
  [    WW ! {fun(ProcState) -> Fun(ProcState, ProcIndex, AA) end, 'workers:broadcast'}
    || {WW, ProcIndex, AA} <- lists:zip3(W, lists:seq(1, length(W)), Args)
  ],
  ok.

%% @spec retrieve(W::worker_pool(), What, Args::List, TimeOut) -> Values::List
%% @doc Return a list of the results of evaluating <code>What</code> in each process of <code>W</code>.
%%
%% If <code>What</code> is a function with arity 0, then it is called with no arguments (of course).<br/>
%% If <code>What</code> is a function with arity 1, then it is called with the current process
%% state.<br/>
%% If <code>What</code> is a function with arity 2, then it is called with the current process state and
%% <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>,
%% where <code>N</code> is the index of the process in <code>W</code>.<br/>
%% If <code>What</code> has arity 3, then the first parameter is the current process state,
%% the second parameter is the index of the process, and the third parameter is
%% <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>.<br/>
%% If <code>What</code> is none of these, then we return the value of 
%% <code>{@link workers:get/3. workers:get}(ProcState, What, undefined)</code> in each worker process.<br/>
%% <br/>
%% If A worker process does not respond after <code>TimeOut</code> time units,
%% then an error tuple is placed in the list.  See {@link set_debug_timeout/2} for the allowable
%% values for <code>TimeOut</code>.
retrieve(W, Fun, Args, TimeOut) ->
  TimeOutMilliSeconds = timeout_val(TimeOut),
  MyPid = self(),
  F0 = case Fun of
    F when is_function(F, 3) -> F;
    F when is_function(F, 2) ->
      fun(ProcState, _ProcIndex, Arg) -> Fun(ProcState, Arg) end;
    F when is_function(F, 1) ->
      fun(ProcState, _ProcIndex, _Arg) -> Fun(ProcState) end;
    F when is_function(F, 0) ->
      fun(_ProcState, _ProcIndex, _Arg) -> Fun() end;
    X ->
      fun(ProcState, _ProcIndex, _Arg) -> workers:get(ProcState, X, undefined) end
  end,
  F1 = fun(ProcState, ProcIndex, Arg) ->
    Outcome = try
      {ok, F0(ProcState, ProcIndex, Arg)}
    catch
      Class:Reason:Stack -> {fail, {Class, Reason, Stack}}
    end,
    V = {'$cs418:workers:retrieve$', ProcIndex, Outcome},
    MyPid ! V,
    ProcState
  end,
  broadcast(W, F1, Args),
  Stuff = [    receive
		 {'$cs418:workers:retrieve$', ProcIndex, {Flag, Outcome}} ->
		   {Flag, ProcIndex, Outcome}
		 after TimeOutMilliSeconds ->
		   misc:msg_dump(
		     "workers:retrieve",
		     [io_lib:format("{'$cs418:workers:retrieve$', ~b, {Flag, Outcome}}",
		                    [ProcIndex])])
	       end
	    || ProcIndex <- lists:seq(1, length(W))
	  ],
  case [ {I, F} || {Flag, I, F} <- Stuff, Flag =:= fail] of
    [] -> [ V || {_Flag, _I, V} <- Stuff]; % no errors
    Bad ->
      {S, ES} = case Bad of [_] -> ["", ""]; _ -> {"s", "es"} end,
      throw({
        fail,
	lists:flatten(io_lib:format("workers:retrieve -- failure~s in worker process~s",
				    [S, ES])),
	[     { lists:nth(ProcIndex, W),
		lists:flatten(io_lib:format("worker ~b", [ProcIndex])),
		Class,
		case Reason of
		  {'workers:get', {pid,_Pid},{not_found,Key}} ->
		    {not_found, Key};
		  OtherReason -> OtherReason
		end,
		lists:takewhile(
		  fun({Module, _, _, _}) -> (Module =/= workers) end,
		  StackTrace)
	      }
	  ||  {ProcIndex, {Class, Reason, StackTrace}} <- Bad
	]
      })
  end.
        

%% @spec retrieve(W::worker_pool(), Fun, X) -> [Value]
%% @doc Each worker evaluates Fun.  X can be a list or a time-out specifier.
%%   If <code>X</code> is a list, then <code>retrieve(W, Fun, X)</code> is equivalent to
%%   <code>{@link workers:retrieve/4 . retrieve}(W, Fun, X, {@link workers:default_debug_timeout/0. default_debug_timout}()).</code><br/>
%%   Otherwise, <code>retrieve(W, Fun,X)</code> is equivalent to
%%   <code>{@link retrieve/4. retrieve}(W, Fun, [ok, ok, ...], TimeOut}</code>,
%%   where the list of <code>ok</code>'s has the same length as <code>W</code>.
retrieve(W, Fun, Args) when is_list(Args) ->
  retrieve(W, Fun, Args, default_debug_timeout());
retrieve(W, Fun, TimeOut) ->
  retrieve(W, Fun, [ ok || _ <- W], timeout_val(TimeOut)).

%% @spec retrieve(W::worker_pool(), X) -> Values
%%    X = (   fun((ProcState::worker_state()) -> term())
%%          | fun((ProcState::worker_state(), N::integer()) -> term())
%%          | term()
%%    ),
%%    Values = [ term() ]
%% @doc
%% Each worker evaluates <code>Fun</code> and the results are returned.
%% The return value of retrieve(W, Fun, Args)
%% is a list whose <code>N</code>th element is the result of applying
%% <code>Fun</code> in the <code>N</code>th process.
%% If
%% <ul>
%%   <li><code>X</code> is a function with arity 1,
%%     then it is called with the current process state.
%%   </li>
%%   <li> <code>X</code> is a function with arity 2,
%%     then the first parameter is the current process state,
%%     and the second parameter is the index of the process.
%%   </li>
%%   <li>Otherwise, <code>X</code> is taken as the key for looking
%%     up a value in the state of each worker process.
%%     If no match is found, then the atom <code>'undefined'</code>
%%     is returned.
%%   </li>
%% </ul>
retrieve(W, X) ->
  F = if
    is_function(X, 1) -> fun(ProcState, _N, _Arg) -> X(ProcState) end;
    is_function(X, 2) -> fun(ProcState, N, _Arg) -> X(ProcState, N) end;
    true ->  % X is the key for looking up a value in the process state
      fun(ProcState, _N, _Arg) -> get(ProcState, X) end
    end,
  retrieve(W, F, W).

%% @spec update(W::worker_pool(), Key::term(), Fun, Args) -> ok
%%    Fun = (   fun((S::worker_state(), Arg) -> term())
%%            | fun((S::worker_state(), N::integer(), Arg) -> term())
%%          | term()
%%    ),
%%    Values = [ term() ]
%% @doc
%% Each worker updates the value in its state (<code>ProcState</code>) associated
%% with <code>Key</code> to the result of applying Fun.  If there is no entry
%% in the current state for <code>Key</code>, then an entry is created.
%% If
%%  <ul>
%     <li><code>Fun</code> has arity 2,
%%      then it is called with the current process state and
%%       <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>,
%%      where <code>N</code> is the index of the process in <code>W</code>.
%%    </li>
%%    <li><code>Fun</code> has arity 3,
%%      then the first parameter is the current process state,
%%      the second parameter is the index of the process, <code>N</code>,
%%      and the third parameter is
%%      <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>.
%%   </li>
%% </ul>
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

%% @spec update(W::worker_pool(), Key::term(), X) -> ok
%%    Fun = (   fun((ProcState::worker_state()) -> term())
%%            | fun((ProcState::worker_state(), N::integer()) -> term())
%%            | [ term() ]
%%    ),
%%    Values = [ term() ]
%% @doc
%% Each worker updates the value in its state (<code>ProcState</code>) associated with
%% <code>Key</code> to the result of applying Fun.  If there is no entry in
%% the current state for <code>Key</code>, then an entry is created.
%% If
%% <ul>
%%   <li><code>X</code> is a function with arity 1, then it is called with the current process state.</li>
%%   <li><code>X</code> is a function with arity 2,
%%     then it is called with the current process state and the index
%%     of the process.
%%   </li>
%%   <li><code>X</code> is a list, then
%%     <code><a href="http://www.erlang.org/doc/man/erlang.html#length-1">length</a>(X)</code>
%%     must be the same as the number of workers in <code>W</code>
%%     (i.e. <code>{@link nworkers/1. nworkers}(W)</code>).
%%     In this case, the value for <code>Key</code> in worker
%%     <code>N</code> is updated to the value of
%%     <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>.
%%   </li>
%% </ul>
update(W, Key, X) ->
  F = if
    is_function(X, 0) -> fun(_ProcState, _N, _A) -> X() end;
    is_function(X, 1) -> fun(ProcState, _N, _A) -> X(ProcState) end;
    is_function(X, 2) -> fun(ProcState, N, _A) -> X(ProcState, N) end;
    is_list(X) -> fun(_ProcState, _N, A) -> A end
  end,
  update(W, Key, F, if is_list(X) -> X; true -> W end).


% rand(M, RandState0), a helper function for random and rlist to make
%   sure that they have the same approach to generating random numbers.
rand(M, RandState0) ->
  if
    is_integer(M) -> rand:uniform_s(M, RandState0);
    is_float(M) ->
      {V, NewState} =  rand:uniform_s(RandState0),
      {M*V, NewState}
  end.


%% @spec random(M, ProcState0) -> { RandomValue, ProcState1 }
%%   M = number(),
%%   ProcState0 = worker_state(),
%%   RandomValue  = number(),
%%   ProcState1 = worker_state()
%% @end
%% @doc Generate a random value.
%%   Parameters:
%%   <ul>
%%      <li> <code>M</code>: Specifies the range for random values ad described below</li>
%%      <li> <code>S0</code>: The process state.</li>
%%   </ul>
%%   Result:
%%   <ul>
%%     <li> <code>RandomValue</code>
%%        <ul>
%%          <li> If <code>is_integer(M)</code>, then <code>RandomValue</code>
%%		will be an integer uniformly chosen from 1..M.
%%	    </li>
%%	    <li> If <code>is_float(M)</code>, then <code>RandomValue</code>
%%		will be an float uniformly chosen in [0, M].
%%	    </li>
%%        </ul>
%%     </li>
%%     <li> <code>S1</code>: The new process state.
%%       <br/>We update the random number generator state so that
%%       successive calls to <code>random</code> will produce different
%%       values.
%%      </li>
%%   </ul>
random(M, ProcState0) ->
  RandState0 = workers:get(ProcState0, '$cs418:workers:randomState$'),
  {RandNum, RandState1} = rand(M, RandState0),
  { RandNum, workers:put(ProcState0, '$cs418:workers:randomState$', RandState1) }.

%% @spec random(N, M, ProcState0) -> { RandomList, ProcState1 }
%%   N = integer(),
%%   M = number(),
%%   ProcState0 = worker_state()
%%   RandomList  = number(),
%%   ProcState1 = worker_state()
%% @end
%% @doc Generate a list of N random values.
%%   <code>RandomList</code> is a list of <code>N</code> random numbers uniformly
%%   chosen according to <code>M</code>.
%%   <ul>
%%     <li> If <code>is_integer(M)</code>, then <code>RandomValue</code>
%%	    will be an integer uniformly chosen from 1..M.
%%     </li>
%%     <li> If <code>is_float(M)</code>, then <code>RandomValue</code>
%%	    will be an float uniformly chosen in [0, M].
%%     </li>
%%   </ul>
%%   The state for the random-number generator is obtained from the worker process
%%   state, <code>ProcState0</code>, and the updated state (new value for the random number
%%   generator state) is returned in <code>ProcState1</code>.
random(N, M, ProcState0) ->
  RandState0 = workers:get(ProcState0, '$cs418:workers:randomState$'),
  {RandList, RandState1} =
    lists:mapfoldl(fun(_, RandState) -> rand(M, RandState) end,
                       RandState0, lists:seq(1, N)),
  {RandList, workers:put(ProcState0, '$cs418:workers:randomState$', RandState1)}.

  

%% @spec rlist(W, N, M, Key) -> ok
%%   W = worker_pool(),
%%   N = integer(),
%%   M = number(),
%%   Key = term()
%% @doc  Generate a pseudo-random list distributed amongst the workers of
%%   worker pool W.  N is the total number of elements in the list.
%%   <ul>
%%     <li> if M is an integer, then each element is uniformly distributed
%%	      in 1..M.</li>
%%     <li> if M is a float, then each element is uniformly distributed
%%	      in [0, M].</li>
%%   </ul>
rlist(W, N, M, Key) -> rlist1({W, length(W)}, N, M, Key).

%% @spec rlist(W, N, Key) -> ok
%%   W = worker_pool(),
%%   N = integer(),
%%   Key = term()
%% @equiv rlist(W, N, 1.0, Key)
rlist(W, N, Key) -> rlist(W, N, 1.0, Key).

% rlist1(W, N, M, Key):
%   Like rlist, we figure out how many elements the first worker should
%   get, send it to the appropriate task, and then make a recursive call
%   to handle the other workers.
rlist1({[], 0}, _N, _M, _Key) -> ok;
rlist1({[W_head | W_tail], NW}, N, M, Key) ->
  N1 = round(N/NW),
  N2 = N - N1,
  W_head ! {(fun(ProcState) -> rlist2(N1, M, ProcState, Key) end), 'workers:rlist'},
  rlist1({W_tail, NW-1}, N2, M, Key).

% rlist2(N, M, ProcState, Key)
%   The worker task for generating a random list.
%   If the worker already has a random-number generator state, use it.
%   Otherwise, create one based on our PID.  That gives each worker a
%   different random sequence.
%   We associate the random list that we create with Key in ProcState.
rlist2(N, M, ProcState0, Key) ->
  {L, ProcState1} = lists:mapfoldl( % generate the random list and a new randomState.
    fun(_, RS) -> random(M, RS) end,
    ProcState0,
    lists:seq(1, N)
  ),
  workers:put(ProcState1, Key, L). % update ProcState

%% @spec seq(W, Lo, Hi, Stride, Key) -> ok
%% @doc Like <code>lists:seq/3</code>, but distributes the list over the workers of <code>W</code>.
%%   Generate a list distributed amongst the workers of
%%   worker pool <code>W</code> that has the elements
%%   <code>Lo</code>...<code>Hi</code> with a stride of <code>Stride</code>
%%   <ul>
%%     <li> <code>W</code> is a worker pool.</li>
%%     <li> <code>Lo</code>, <code>Hi</code>, and <code>Stride</code> must
%%	      be integers with <code>Lo =&lt; Hi</code> and <code>Stride /= 0</code>.
%%     </li>
%%     <li> The list produced by <code>seq</code> is associated with the
%%	      key <code>Key</code>.
%%     </li>
%%   </ul>
%%   For example, if <code>W</code> has 4 worker processes, then <code>seq(W, 0, 10, 1, foo)</code>
%%   will associate
%%   <ul>
%%     <li><code>[0,1]</code> with <code>foo</code> in worker <code>1</code>,</li>
%%     <li><code>[2,3,4]</code> with <code>foo</code> in worker <code>2</code>,</li>
%%     <li><code>[5,6,9]</code> with <code>foo</code> in worker <code>3</code>, and</li>
%%     <li><code>[8,9,10]</code> with <code>foo</code> in worker <code>4</code>.</li>
%%   </ul>
seq(W, Lo, Hi, Stride, Key)
    when     is_integer(Lo) and is_integer(Hi) and is_integer(Stride)
         and (Stride /= 0) and ((Hi-Lo)*Stride >= 0) ->
  workers:update(W, Key,
    fun(_ProcState, {L, H}) ->
      LL = L + misc:mod((Lo - L), Stride),
      HH = H-1,
      if
        ((HH-LL)*Stride >= 0) -> lists:seq(LL, HH, Stride);
	true -> []
      end
    end, misc:intervals(Lo, Hi+1, nworkers(W))
  ).
%% @spec seq(W, Lo, Hi, Key) -> ok
%% @equiv seq(W, Lo, Hi, 1, Key)
seq(W, Lo, Hi, Key) -> seq(W, Lo, Hi, 1, Key).

%% @spec set_debug_timeout(W, T) -> ok
%%   W = worker_pool(),
%%   T = integer() | float() | default | infinity
%% @doc Set the time-out for receive operations in the workers package to T.
%%   If T is an integer, it specifies the time-out in milliseconds.<br/>
%%   If T is a float, it specifies the time-out in seconds.<br/>
%%   If T is the atom 'default', the value from default_debug_timeout() is used.<br/>
%%   If T is the atom 'infinity', then operations never time-out.
set_debug_timeout(W, T) ->
  V = timeout_val(T),
  workers:update(W, '$cs418:workers:debug_timeout$', fun() -> V end).

%% @spec get_debug_timeout(ProcState) -> integer()
%%   ProcState = worker_state()
%% @doc return the debugging time-out for receives
get_debug_timeout(ProcState) ->
  workers:get(ProcState, '$cs418:workers:debug_timeout$', fun() -> default_debug_timeout() end).

%% @spec default_debug_timeout() -> 5000
%% @doc  The default time-out for debugging operations in the workers package.
%%   Currently, this is set to 5000 (i.e. 5 seconds), but I might change it.
default_debug_timeout() -> 5000.

timeout_val(default) -> default_debug_timeout;
timeout_val(infinity) -> infinity; 
timeout_val(MilliSeconds) when is_integer(MilliSeconds) -> MilliSeconds; 
timeout_val(Seconds) when is_float(Seconds) -> round(1000*Seconds).
