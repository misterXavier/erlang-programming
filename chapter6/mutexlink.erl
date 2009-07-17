-module(mutexlink).
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
  process_flag(trap_exit, true),
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
  try link(Pid) of
    true ->
      receive
        {signal, Pid} ->
          unlink(Pid),
          free();
        {exit, Pid, _ } ->
          io:format('processo ~w crasheou', [Pid]),
          free()
      end
  catch
    _ -> free()
  end.

terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
    after
      0 -> ok
  end.
