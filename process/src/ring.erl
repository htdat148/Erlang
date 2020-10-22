-module(ring).
-export([start/2]).
-export([start/3]).

loop(0, _NextPid) ->
    io:format("[process ~p] terminate ~n", [self()]);

loop(M, NextPid) ->
    receive
	start_transfer ->
	    io:format("[process ~p] starts transfer ~n", [self()]),
	    NextPid ! {propagate, self()},
	    loop(M, NextPid);
	 {propagate, From }->
	    io:format("[process ~p] propagate throughtout [process ~p]~n", [From, self()]),
	    NextPid ! {propagate, self()},
	    loop(M-1, NextPid)
    end.


start(N, M) ->
    spawn(?MODULE,start,[N, M, start]).


start(N, M, start) ->
    io:format("[process ~p] was created~n", [self()]),
    ChildPid = spawn(?MODULE, start, [N-1, M, self()]),
    loop(M, ChildPid);

start(1, M, ParentPid) ->
    io:format("[process ~p] was created~n", [self()]),
    ParentPid ! start_transfer,
    loop(M, ParentPid);

start(N, M, ParentPid) ->
    io:format("[process ~p] was created~n", [self()]),
    ChildPid = spawn(?MODULE, start, [N-1, M, ParentPid]),
    loop(M, ChildPid).


