%% master process solution
%% spawn a master process then create all other processes
-module(ring2).
-export([start/1, create/1, create/2]).
-export([loop/1]).

start(N) ->
    spawn(?MODULE, create, [N]).

create(N) ->
    create(N, self()).

create(1, NextPid) ->
    loop(NextPid);
create(N, NextPid) ->
    Prev = spawn(?MODULE, loop, [NextPid]),
    create(N-1, Prev).

loop(NextPid) ->
    io:format("current ~p process propagate to ~p process ~n", [self(), NextPid]).
