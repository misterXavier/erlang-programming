%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(fsm).
-export([turn_on/0, idle/0,ringing/1, init/0]).

turn_on() ->
  spawn(fsm, init, []).

init() ->
  start_event_manager(),
  idle().

idle() ->
  receive
    {Number, incoming} ->
      start_ringing(),
      ringing(Number);
    off_hook ->
      start_tone(),
      dial()
  end.

ringing(Number) ->
  receive
    {Number, other_on_hook} ->
      stop_ringing(),
      idle();
    {Number, off_hook} ->
      stop_ringing(),
      connected(Number)
  end.

dial() ->
  receive on_hook -> idle() end.

connected(_Number) ->
  receive on_hook -> idle() end.

start_ringing() ->
  send_event(start_ringing).

start_tone()    ->
  send_event(start_tone).

stop_ringing()  ->
  send_event(stop_ringing).

start_event_manager() ->
  register(fsm_manager, spawn(event_manager, init, [[]])).

send_event(Event) ->
  fsm_manager ! {request, self(), {send_event, Event}},
  receive {reply, Reply} -> Reply end.