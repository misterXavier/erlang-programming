-module(math1_tc).
-export([sum/1, sum/2]).

sum(N) ->
  sum_n(N, 0).
sum_n(0, Sum) ->
  Sum;
sum_n(N, Sum) ->
  sum_n(N-1, Sum+N).

sum(N, M) when N > M ->
  throw({nBiggerThanM, N, M});
sum(N, M) ->
  sum_range(N, M, 0).

sum_range(N, M, Acc) when N == M ->
  Acc+N;
sum_range(N, M, Acc) ->
  sum_range(N+1, M, Acc+N).