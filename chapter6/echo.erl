-module(echo).
-export([start/0, print/1, quit/0, echo_server/0]).

start() ->
  register(echo, spawn_link(echo, echo_server, [])).

print(Msg) ->
  echo ! {message, Msg}.

quit() ->
  exit(requested_by_user).

echo_server() ->
  receive
    {message, Msg} ->
      io:format("Received Message: ~p~n", [Msg]),
      echo_server()
  end.

