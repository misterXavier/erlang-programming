-module(print1).
-export([print_range/1, print_even/1]).

print_range(0) ->
  ok;
print_range(N) ->
  print_range(N-1),
  io:format("Number: ~p~n", [N]).

print_even(0) ->
  ok;
print_even(N) when N rem 2 == 0 ->
  print_even(N-2),
  io:format("Even Number: ~p~n", [N]).