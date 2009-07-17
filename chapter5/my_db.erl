-module(my_db).
-export([start/0, stop/0, write/2, read/1, delete/1, match/1]).
-export([init/0]).

% Init

start() ->
  register(my_db, spawn(my_db, init, [])).

init() ->
  loop([]).

% Client

stop() ->
  io:format("Quitting now~n"),
  call({command, quit}).

write(Key, Element) ->
  call({write, Key, Element}).

delete(Key) ->
  call({delete, Key}).

read(Key) ->
  call({read, Key}).

match(Element) ->
  call({match, Element}).

call(Msg) ->
  my_db ! {request, self(), Msg},
  receive {reply, Reply} -> Reply end.

% Server

loop(Db) ->
  receive
    {request, Pid, {write, Key, Value}} ->
      NewDb = write(Key, Value, Db),
      reply(Pid, ok),
      loop(NewDb);
    {request, Pid, {delete, Key}} ->
      NewDb = delete(Key, Db),
      reply(Pid, ok),
      loop(NewDb);
    {request, Pid, {read, Key}} ->
      reply(Pid, read(Key, Db)),
      loop(Db);
    {request, Pid, {match, Element}} ->
      reply(Pid, match(Element, Db, [])),
      loop(Db);
    {request, Pid, {command, quit}} ->
      reply(Pid, ok)
  end.

write(Key, Element, Db) ->
  [{Key, Element}|Db].

delete(Key, Db) ->
  lists:keydelete(Key, 1, Db).

read(Key, Db) ->
  case lists:keysearch(Key, 1, Db) of
    {value, {Key, Element}} -> {ok, Element};
    _ -> {error, instance}
  end.

match(_Element, [], Matches) -> Matches;
match(Element, [H|T], Matches) ->
  case H of
    {Key, Element} -> match(Element, T, [Key|Matches]);
    _ -> match(Element, T, Matches)
  end.

reply(Pid, Msg) ->
  Pid ! {reply, Msg}.