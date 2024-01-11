% examples.erl -- demonstrate some of the functions in the cs418 module library

-module(examples).

-export [sum/2, sum_test/0, sum_time/2, sum_time/0].
-export [prefix_sum/3, prefix_sum_test/0, prefix_sum_time/2,
         prefix_sum_time/0, prefix_sum_verbose/3, prefix_sum_verbose_test/0].


% module wtree

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                          %
%  Examples of reduce                                                      %
%                                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sum(W, Key) ->
  wtree:reduce(W,
    fun(ProcState) -> lists:sum(wtree:get(ProcState, Key)) end,  % Leaf
    fun(Left, Right) -> Left + Right end % Combine
  ).


% sum_test(): a simple test case for sum/2.  We create the list
% [0, 1, ..., 20].  We compute the sum of the elements of this list
% (should be 210) using lists:sum and sum/2.  They should agree.
sum_test() ->
  W = wtree:create(4),
  MyList = lists:seq(0,20),
  workers:update(W, sum_data_1, misc:cut(MyList, W)),
  ParSum = sum(W, sum_data_1),
  SeqSum = lists:sum(MyList),
  Status = case ParSum of
    SeqSum ->
      io:format("sum_test: passed.  The sum is ~b~n", [ParSum]),
      ok;
    _ ->
      io:format("sum_test: FAILED.  Got ~b.  Expected ~b~n", [ParSum, SeqSum]),
      fail
  end,
  wtree:reap(W),  % clean-up: terminate the workers
  Status.


% Let's check the speed-up using sum with P processes for a list with N elements.
sum_time(P, N) ->
  W = wtree:create(P),
  wtree:rlist(W, N, 1000000, sum_data_2),
  wtree:barrier(W),  % make sure the rlist computations have finished
  ParTime = time_it:t(fun() -> sum(W, sum_data_2) end),
  ParSum = sum(W, sum_data_2),
  MyList = lists:append(workers:retrieve(W, sum_data_2)),
  SeqTime = time_it:t(fun() -> lists:sum(MyList) end),
  SeqSum = lists:sum(MyList),
  Status = case ParSum of
    SeqSum ->
      io:format("sum_time: passed.  The sum is ~b~n", [ParSum]),
      io:format("  timing stats for parallel version: ~w~n", [ParTime]),
      io:format("  timing stats for sequential version: ~w~n", [SeqTime]),
      SpeedUp = element(2, lists:keyfind(mean, 1, SeqTime)) /
                element(2, lists:keyfind(mean, 1, ParTime)),
      io:format("  speed-up: ~6.3f~n", [SpeedUp]);
    _ ->
      io:format("sum_time: FAILED.  Got sum of ~b.  Expected ~b~n", [ParSum, SeqSum]),
      fail
  end,
  wtree:reap(W),  % clean-up: terminate the workers
  Status.

% A few cases for sum_time.  When N is "small" (i.e. 1000), we expect a
%   slow-down because of the communication overhead for the parallel version.
%   For a "large" N (i.e. 1000000), we expect to see a speed-up.
sum_time() ->
  sum_time( 4,    1000),
  sum_time( 4, 1000000),
  sum_time(16,    1000),
  sum_time(16, 1000000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                          %
%  Examples of scan                                                        %
%                                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


prefix_sum(W, SrcKey, DstKey) ->
  Leaf1 = fun(ProcState) ->
    lists:sum(wtree:get(ProcState, SrcKey))
  end,
  Leaf2 = fun(ProcState, AccIn) ->
    Src = wtree:get(ProcState, SrcKey),  % get the original list
    Result = misc:cumsum(AccIn, Src),    % compute the cummulative sum
    wtree:put(ProcState, DstKey, Result) % save the result -- must be the last expression
  end,                                   %   in the Leaf2 function
  Combine = fun(Left, Right) -> Left+Right end,
  wtree:scan(W, Leaf1, Leaf2, Combine, 0).


% I'll also provide a "verbose" version so you can see what's going on.
prefix_sum_verbose(W, SrcKey, DstKey) ->
  Leaf1 = fun(ProcState) ->
    Src = wtree:get(ProcState, SrcKey),
    Result = lists:sum(Src),
    io:format("~w Leaf1(Src=~w) -> ~w~n", [self(), Src, Result]),
    Result
  end,
  Leaf2 = fun(ProcState, AccIn) ->
    Src = wtree:get(ProcState, SrcKey),  % get the original list
    Result = misc:cumsum(AccIn, Src),    % compute the cummulative sum
    io:format("~w Leaf2(AccIn=~w, Src=~w) -> put(~w, ~w)~n",
      [self(), AccIn, Src, DstKey, Result]),
    wtree:put(ProcState, DstKey, Result) % save the result -- must be the last expression
  end,                                   %   in the Leaf2 function
  Combine = fun(Left, Right) ->
    Result = Left+Right,
    io:format("~w Combine(~w, ~w) -> ~w~n", [self(), Left, Right, Result]),
    Result
  end,
  wtree:scan(W, Leaf1, Leaf2, Combine, 0).


% The test cases are quick rewrites of those for sum

% prefix_sum_test(): a simple test case for prefix_sum/2.  We create
% the list [0, 1, ..., 20].  We compute the prefix sum of the elements of
% this list (should be 210) using lists:mapfoldl and sum/2.  They should
% agree.
prefix_sum_test() ->
  W = wtree:create(4),
  MyList = lists:seq(1,21,2),
  workers:update(W, sum_data_1, misc:cut(MyList, W)),
  prefix_sum(W, sum_data_1, sum_data_2),
  % The results is stored on the workers.  The key is sum_data_2.
  % Let's get it.
  ParSum = lists:append(workers:retrieve(W, 'sum_data_2')),

  % now, try the sequential version
  SeqSum = misc:cumsum(MyList),
  Status = case ParSum of
    SeqSum ->
      io:format("prefix_sum_test: passed.  The sum is ~w~n", [ParSum]),
      ok;
    _ ->
      io:format("sum_test: FAILED.  Got ~w.  Expected ~w~n", [ParSum, SeqSum]),
      fail
  end,
  wtree:reap(W),  % clean-up: terminate the workers
  Status.

prefix_sum_verbose_test() ->
  W = wtree:create(4),
  io:format("The workers are: ~w~n", [W]),
  MyList = misc:rlist(20, 50),
  io:format("The original lists is: ~w~n", [MyList]),
  workers:update(W, 'raw_data', misc:cut(MyList, W)),
  prefix_sum_verbose(W, 'raw_data', 'cooked_data'),
  io:format("The final answer is ~w~n",
            [lists:append(workers:retrieve(W, 'cooked_data'))]).

% Let's check the speed-up using sum with P processes for a list with N elements.
prefix_sum_time(P, N) ->
  W = wtree:create(P),
  wtree:rlist(W, N, 1000000, brontosaurus),
  wtree:barrier(W),  % make sure the rlist computations have finished
  ParTime = time_it:t(fun() -> prefix_sum(W, brontosaurus, banana) end),
  ParSum = lists:append(workers:retrieve(W, banana)),
  OriginalList = lists:append(workers:retrieve(W, brontosaurus)),
  SeqTime = time_it:t(fun() -> misc:cumsum(OriginalList) end),
  SeqSum = misc:cumsum(OriginalList),

  % this time, we won't print the sums because they're *really* long lists.
  Status = case ParSum of
    SeqSum ->
      io:format("sum_time: passed.  The sums match~n"),
      io:format("  timing stats for parallel version: ~w~n", [ParTime]),
      io:format("  timing stats for sequential version: ~w~n", [SeqTime]),
      SpeedUp = element(2, lists:keyfind(mean, 1, SeqTime)) /
                element(2, lists:keyfind(mean, 1, ParTime)),
      io:format("  speed-up: ~6.3f~n", [SpeedUp]);
    _ ->
      io:format("sum_time: FAILED.  The sums don't match. :(~n"),
      fail
  end,
  wtree:reap(W),  % clean-up: terminate the workers
  Status.

% A few cases for sum_time.  When N is "small" (i.e. 1000), we expect a
%   slow-down because of the communication overhead for the parallel version.
%   For a "large" N (i.e. 1000000), we expect to see a speed-up.
prefix_sum_time() ->
  prefix_sum_time( 4,    1000),
  prefix_sum_time( 4, 1000000),
  prefix_sum_time(16,    1000),
  prefix_sum_time(16, 1000000).
