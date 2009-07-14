-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2, match_tc/2]).

new() -> [].

destroy(_Db) -> ok.

write(Key, Element, Db) ->
  [{Key, Element}|Db].

delete(_Key, []) -> [];
delete(Key, [H|T]) ->
  case H of
    {Key, _} -> delete(Key, T);
    _ -> [H|delete(Key, T)]
  end.

read(_Key, []) -> [];
read(Key, [H|T]) ->
  case H of
    {Key, Val} -> Val;
    _ -> read(Key, T)
  end.

match(_Element, []) -> [];
match(Element, [H|T]) ->
  case H of
    {Key, Element} -> [Key|match(Element, T)];
    _ -> match(Element, T)
  end.

match_tc(Element, Db) ->
  match_tc(Element, Db, []).
match_tc(_Element, [], Matches) -> Matches;
match_tc(Element, [H|T], Matches) ->
  case H of
    {Key, Element} -> match_tc(Element, T, [Key|Matches]);
    _ -> match_tc(Element, T, Matches)
  end.