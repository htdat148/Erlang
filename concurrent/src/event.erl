-module(event).
-export([start/3, start_link/3, cancel/1]).
-export([init/4, loop/2]).
-record(state, {server,
                name="",
                to_go=0}).

%%% Public interface
start(MasterPid, EventName, DateTime) ->
    spawn(?MODULE, init, [self(), MasterPid, EventName, DateTime]).

start_link(MasterPid, EventName, DateTime) ->
    spawn_link(?MODULE, init, [self(), MasterPid, EventName, DateTime]).

cancel(Pid) ->
    %% Monitor in case the process is already dead
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    end.

%%% Event's innards
init(Server, MasterPid, EventName, DateTime) ->
    loop(MasterPid, #state{server=Server,
			   name=EventName,
			   to_go=time_to_go(DateTime)}).

%% Loop uses a list for times in order to go around the ~49 days limit
%% on timeouts.
loop(MasterPid,
     S = #state{server=Server, to_go=[T|Next]}) ->
    receive
        {Server, Ref, cancel} ->
	    io:format("Client ~p cancels event", [MasterPid]),
            Server ! {Ref, ok}
    after T*1000 ->
        if Next =:= [] ->
            Server ! {event_timeout, MasterPid, S#state.name};
           Next =/= [] ->
            loop(MasterPid, S#state{to_go=Next})
        end
    end.

%%% private functions
time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
           calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0  -> ToGo;
              ToGo =< 0 -> 0
           end,
    normalize(Secs).

%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used
normalize(N) ->
    Limit = 49*24*60*60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].
