-module(mutexmonitor).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
  register(mutex, spawn(?MODULE, init, [])).

stop() ->
  mutex ! stop.

wait() ->
  mutex ! {wait, self()},
  receive ok -> ok end.

signal() ->
  mutex ! {signal, self()}, ok.

init() ->
  free().

free() ->
  receive
    {wait, Pid} ->
      Pid ! ok,
      busy(Pid);
    stop ->
      terminate()
  end.

busy(Pid) ->
  Ref = erlang:monitor(process, Pid),
  receive
    {signal, Pid} ->
      erlang:demonitor(Ref, [flush]),
      free();
    {'DOWN', Ref, process, Pid, _ } ->
      io:format('processo ~w crasheou', [Pid]),
      free()
  end.

terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
    after
      0 -> ok
  end.
