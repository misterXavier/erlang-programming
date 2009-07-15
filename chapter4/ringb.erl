-module(ring).
-export([start/3, start/4]).

start(Times, Processes, Msg) ->
  start(Times, Processes, Msg, self()).

start(Times, Processes, Msg, First) when Processes == 1 ->
  io:format("Node ~w received (~B p ~B). * Last~n", [self(), Times, Processes]),
  First ! {message, Times, Msg},
  serve_requests(First);

start(Times, Processes, Msg, First) ->
  io:format("Node ~w received (~B p ~B).~n", [self(), Times, Processes]),
  Next = spawn(ring, start, [Times, Processes-1, Msg, First]),
  serve_requests(Next).

serve_requests(Next) ->
  receive
    {message, 1, Msg} ->
      io:format("Node ~w received ~p, last time being received. Telling ~w to quit~n", [self(), Msg, Next]),
      Next ! {command, quit};
    {message, Times, Msg} ->
      io:format("Node ~w received ~p, ~B times. Forwarding to ~w~n", [self(), Msg, Times, Next]),
      Next ! {message, Times-1, Msg},
      serve_requests(Next);
    {command, quit} ->
      io:format("Node ~w quitting now. Telling ~w to quit~n", [self(), Next]),
      Next ! {command, quit}
  end.