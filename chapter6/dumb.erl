-module(dumb).
-export([init/0, loop/0]).

init() ->
  spawn(dumb, loop, []).

loop() ->
  receive
    hold ->
      mutex ! {wait, self()},
      response(),
      loop();
    signal ->
      mutex ! {signal, self()},
      response(),
      loop();
    stop ->
      mutex ! stop
  end.

response() ->
  receive Val -> Val end.