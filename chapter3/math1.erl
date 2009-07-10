-module(math1).
-export([sum/1, sum/2, sum_tc/1, sum_tc/2]).

sum(1) ->
  1;
sum(N) ->
  N + sum(N-1).

sum(N, M) when N > M ->
  throw({badarg, "N não pode ser maior"});
sum(N, M) when N == M ->
  N;
sum(N, M) ->
  N + sum(N+1, M).
