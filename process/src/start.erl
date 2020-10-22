%% Write a function which starts N processes in a star, and sends a message to each of them M times.
%% After the messages have been sent the processes should terminate gracefully.
-module(start).

-export([create/3]).
-export([loop/1]).
-compile(export_all).


create(N, M, Message) ->
    spawn(fun() -> master_start(N, M, Message)
	  end).


master_start(N, M, Message) ->
    ChildPid = create_child(N, M),
    send_to_child(ChildPid, Message, M).


send_to_child(ChildPid, Message, M) ->
    Fun = fun(_) -> [Pid ! {Message, self()} || Pid <- ChildPid] end,
    lists:foreach(Fun, lists:seq(1, M)).


create_child(N, M) ->
    lists:map(fun(_) -> 
		      spawn(fun() -> loop(M) end)
	      end, lists:seq(1, N)).


loop(0) ->
    ok;
loop(M) ->
    receive
	{Message, From} ->
	    io:format("Child ~p process receives ~p from ~p~n", [self(), Message, From]),
	    loop(M-1)
    end.
