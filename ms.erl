%% Master and Slaves, error handling
%% http://erlang.org/course/exercises.html
%% This problem illustrates a situation where we have a process (the master) which supervises other processes (the slaves).
%% In a real example the slave could, for example, be controlling different hardware units.
%% The master's job is to ensure that all the slave processes are alive.
%% If a slave crashes (maybe because of a software fault), the master is to recreate the failed slave.
-module(ms).

-export([start/1, to_slave/2]).
-export([master/1, client/1]).


%% start N child processes
start(N) ->
    register(master, spawn(fun() -> start_master_slave(N)
			   end)).

start_master_slave(N) ->

    process_flag(trap_exit, true),
    ChildList = lists:map(fun(X) ->
				  Pid = spawn_link(?MODULE, client, [X]),
				  io:format("Child ~p is created~n", [Pid]),
				  {X, Pid}
			  end, lists:seq(1, N)),
    master(ChildList).


to_slave(Mesg, Id) ->
    master ! {Mesg, Id, self()},
    receive
	ok -> ok;
	{error_id, Id} -> {error_id, Id}
    end.

check_id(Id, ChildList) -> 
    lists:keyfind(Id, 1, ChildList).
    
master(ChildList) ->
    receive
	{'EXIT', From, {normal, X}} ->
	    NewChild = spawn_link(?MODULE, client, [X]),
	    io:format("Client ~p is death, create a new one ~p ~n", [From, NewChild]),
	    RemoveDiedChild = lists:keydelete(X, 1, ChildList),
	    NewChildList = [{X, NewChild} | RemoveDiedChild],
	    master(NewChildList);
	{Msg, Id, From} ->
	    case check_id(Id, ChildList) of
		false ->
		    From ! {error_id, Id},
		    master(ChildList);
		{Id, Pid} ->
		    Pid ! {Msg, self()},
		    From ! ok,
		    master(ChildList)
	    end	    
    end.

client(X) ->
    receive
	{die, _From} ->
	    io:format("Client ~p terminated~n", [X]),
	    exit({normal, X});
	{Msg,_From} ->
	    io:format("Client ~p receives message ~p~n", [X,Msg]),
	    client(X)
    end.
	    
	
	
	    
	    
