%% =====================================================================
%% @copyright 2011 Mark R. Greenstreet
%% @author Mark R. Greenstreet <mrg@cs.ubc.ca>
%% @end
%% =====================================================================
%%
%% @doc stat - simple statistical function.  Currently, this module
%%   provides an <code>accum</code> function for accumulating samples,
%%   and functions for computing the mean and standard deviation from
%%   such accumulations.  I might add more statistical and data analysis
%%   functions at a later date, but I don't have any plans to do so
%%   immediately.

-module stat.
-export [accum/1, accum/2, mean/1, std/1, std/2].

-record(stat_acc, {s0, s1, s2}).

new_acc() -> #stat_acc{s0=0, s1=0, s2=0}.

new_acc(X) when is_number(X) -> #stat_acc{s0=1, s1=X, s2=X*X}.

new_acc(X, #stat_acc{s0=S0, s1=S1, s2=S2}) ->
  #stat_acc{s0=S0+1, s1=S1+X, s2=S2+(X*X)}.
  

%% @spec accum(Data, Acc) -> accumulator()
%%   Data = [ number() ],
%%   Acc = accumulator()
%% @doc Add the sample from <code>Data</code> to the accumulator
%%    <code>Acc</code>.
%%   Return the resulting accumulator.
accum([], Acc) -> Acc;
accum([X | T], Acc) -> accum(T, new_acc(X, Acc));

%% @spec accum(X, Acc) -> accumulator()
%%   X = number(),
%%   Acc = accumulator()
%% @doc Add the sample <code>X</code> to the accumulator <code>Acc</code>.
%%   Return the resulting accumulator.
accum(X, Acc) when is_number(X) -> new_acc(X, Acc).

%% @spec accum(Data) -> accumulator()
%%   Data = number() | [ number() ]
%% @doc Create a new accumulator using <code>Data</code>
%%   If <code>Data</code> is a number, an accumulator is created with
%%   that number as it's one sample.  If <code>Data</code> is a list
%%   of numbers, then an accumulator is created with all of the samples
%%   from the list.

accum([]) -> new_acc();
accum([X | T]) -> accum(T, new_acc(X));
accum(X) when is_number(X) -> new_acc(X).


%% @spec mean(X) -> number() | undefined
%%   X = accumulator() | number() | [ number() ]
%% @doc Compute the mean of the samples in <code>X</code>.
%%   If <code>X</code> an accumulator, then the mean is computed for
%%   the samples that were used to generate <code>X</code>.
%%   If <code>X</code> is a number or a list of numbers, it is used
%%   to create an accumulator (see <code>accum/1</code>) and then the
%%   mean is calculated.
%%   If there are no samples in the accumulator of the list, then the
%%   atom <code>'undefined'</code> is returned.
mean(#stat_acc{s0=0}) -> undefined;
mean(#stat_acc{s0=N, s1=S1}) -> S1/N;
mean(X) when (is_list(X) or is_number(X)) -> mean(accum(X)).

%% @spec std(X, SampleOrPopulation) -> number() | undefined
%%   X = accumulator() | number | [number() ],
%%   SampleOrPopulation = sample | population
%% @doc Compute the standard deviation of the samples in <code>X</code>.
%%   If <code>X</code> an accumulator, then the standard deviation is
%%   computed for the samples that were used to generate <code>X</code>.
%%   If <code>X</code> is a number or a list of numbers, it is used
%%   to create an accumulator (see <code>accum/1</code>) and then the
%%   standard deviation is calculated.
%%   The parameter <code>SampleOrPopulation</code> specifies whether
%%   to compute a
%%   <a href="http://en.wikipedia.org/wiki/Standard_deviation">sample
%%   or population standard deviation</a>.  If you're not sure which
%%   you want, you probably want <code>'sample'</code>.
%%   For a <code>'sample'</code> standard-deviation, there must be at
%%   least two samples, or the result is the atom 'undefined'.
%%   For a <code>'population'</code> standard-deviation, there must be at
%%   least one sample, or the result is the atom 'undefined'.
std(#stat_acc{s0=0}, _) -> undefined;
std(#stat_acc{s0=1}, population) -> 0;
std(#stat_acc{s0=1}, sample) -> undefined;
std(#stat_acc{s0=N, s1=S1, s2 = S2}, P_or_S) ->
  M = case P_or_S of population -> N; sample -> N-1 end,
  D = S2 - (S1*S1/N),
  if
    (D > 0)  -> math:sqrt(D/M);
    (D =< 0) -> 0.0  % avoid arithmetic trap if std is so small that D < 0
  end;               %   due to rounding errors.

std(X, S_or_P) when is_list(X) -> std(accum(X), S_or_P).
  
%% @spec std(X) -> number() | undefined
%% @equiv std(X, sample)
std(X) -> std(X, sample).
