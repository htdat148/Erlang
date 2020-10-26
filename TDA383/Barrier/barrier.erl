-module(barrier).
-export([init/1, wait/1]).

%% Initialize barrier for number expected process
init(Expected) ->
    spawn(fun() ->
		  barrier(0, Expected, [])
	  end).

%% event loop of barrier for Expected processes
%% Arrived: number of processes arrived so far
%% PidRefs: list of {Pid, Ref} of processes arrived so far
%% all processes arrived
barrier(Arrived, Expected, PidRefs)
    when Arrived =:= Expected ->
    [To ! {continue, Ref} || {To, Ref} <- PidRefs],
    %% reset barrier
    barrier(0, Expected, []);

%% still waiting some processes
barrier(Arrived, Expected, PidRefs) ->
    receive
	{arrived, From, Ref} ->
	    barrier(Arrived+1, Expected, [{From, Ref}|PidRefs])
    end.

%% block at barrier until all processes have reached it
wait(Barrier) ->
    Ref = make_ref(),
    %% notify barrier of arival
    Barrier ! {arrived, self(), Ref},
    receive
	{continue, Ref} ->
	    through
    end.
