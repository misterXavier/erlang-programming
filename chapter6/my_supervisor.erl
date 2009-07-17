-module(my_supervisor).
-export([start_link/2, stop/1, start_child/5, stop_child/2]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
  register(Name, spawn_link(my_supervisor, init, [ChildSpecList])), ok.

start_child(Name, M, F, A, T) ->
  Name ! {start_child, self(), {M, F, A, T}},
  receive {reply, Reply} -> Reply end.

stop_child(Name, Id) ->
  Name ! {stop_child, Id}.

init(ChildSpecList) ->
  process_flag(trap_exit, true),
  loop(start_children(ChildSpecList)).

start_children(ChildSpecList) -> start_children(ChildSpecList, 0).

start_children([], _LastId) -> [];
start_children([{M, F, A, T} | ChildSpecList], LastId) ->
  case (catch spawn_link(M,F,A)) of
    {'EXIT', _} ->
      start_children(ChildSpecList);
    Pid ->
      {Hour, Min, _} = time(),
      [{LastId + 1, Pid, {0, {Hour, Min}}, {M, F, A, T}}|start_children(ChildSpecList, LastId + 1)]
  end.

%% The loop of the supervisor waits in a receive clause for EXIT and stop messages.
%% If a child terminates, the supervisor receives the EXIT signal and restarts the terminated
%% child, replacing its entry in the list of children stored in the ChildList variable:

start_child({M, F, A, T}, ChildList) ->
  {LastId, _, _, _} = lists:max(ChildList),
  Pid = spawn_link(M,F,A),
  {Hour, Min, _} = time(),
  {{LastId + 1, Pid}, [{LastId + 1, Pid, {0, {Hour, Min}}, {M, F, A, T}}|ChildList]}.

stop_child_with_id(Id, ChildList) ->
  {value, {Id, Pid, _, _}} = lists:keysearch(Id, 1, ChildList),
  unlink(Pid),
  exit(Pid, kill),
  lists:keydelete(Id, 1, ChildList).

restart_child(Pid, ChildList, Reason) ->
  {value, {Id, Pid, {Times, {Hour, Min}}, {M, F, A, T}}} = lists:keysearch(Pid, 2, ChildList),
  if
    {T, Reason} == {transient, normal} ->
      lists:keydelete(Id,1,ChildList);
    true ->
      case time() of
        {Hour, Min, _} ->
          NewTimes = Times + 1,
          NewHour = Hour, NewMin = Min;
        {NewHour, NewMin, _} ->
          NewTimes = 0
      end,
      if
        NewTimes >= 5 ->
          io:format('above treshold, nooot here'),
          lists:keydelete(Id,1,ChildList);
        true ->
          NewPid = spawn_link(M,F,A),
          [{Id, NewPid, {NewTimes, {NewHour, NewMin}}, {M, F, A, T}}|lists:keydelete(Id,1,ChildList)]
      end
  end.

loop(ChildList) ->
  receive
    {'EXIT', Pid, Reason} ->
      NewChildList = restart_child(Pid, ChildList, Reason),
      loop(NewChildList);
    {start_child, From, Child} ->
      {Reply, NewChildList} = start_child(Child, ChildList),
      From ! {reply, Reply},
      loop(NewChildList);
    {stop_child, Id} ->
      loop(stop_child_with_id(Id, ChildList));
    {stop, From}  ->
      From ! {reply, terminate(ChildList)}
  end.

%% We stop the supervisor by calling the synchronous client function stop/0. Upon receiving the
%% stop message, the supervisor runs through the ChildList, terminating the children one by one.
%% Having terminated all the children, the atom ok is returned to the process that initiated
%% the stop call:

stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end.

terminate([{Pid, _} | ChildList]) ->
  exit(Pid, kill),
  terminate(ChildList);
terminate(_ChildList) -> ok.
