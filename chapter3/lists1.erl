-module(lists1).
-export([create/1, reverse_create/1, filter/2, filter_tr/2, filter_tc/3, reverse/1]).

create(0) ->
  [];
create(N) ->
  create(N-1) ++ [N].

reverse_create(0) ->
  [];
reverse_create(N) ->
  [N] ++ reverse_create(N-1).

filter(List, Filter) ->
  [Y || Y<-List, Y=<Filter].

filter_tr([], _Filter) -> [];
filter_tr([H|T], Filter) when H =< Filter ->
  [H|filter_tr(T, Filter)];
filter_tr([_|T], Filter) ->
  filter_tr(T, Filter).

filter_tc([], _Filter, Matches) -> Matches;
filter_tc([H|T], Filter, Matches) when H =< Filter ->
  filter_tc(T, Filter, Matches ++ [H]);
filter_tc([_|T], Filter, Matches) ->
  filter_tc(T, Filter, Matches).

reverse(List) -> reverse(List, []).
reverse([], List) -> List;
reverse([H|T], List) ->
  reverse(T, [H|List]).

concatenate([])
