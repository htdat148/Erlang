-module(test_event).

-export([test/1]).

-import(evserv, [start/0, terminate/0,
		 start_link/0, subscribe/1,
		 add_event/3, add_event2/3,
		 cancel/1, listen/1]).

test(NumberClients) ->
    FutureDateTime = {{2020,9,27},{23,40,10}}, %30 second timeout
    NextMinuteDataTime = {{2020,9,27},{23,40,50}},
    start_link(), 
    ClientDict = add_subscriber(NumberClients, orddict:new()),
    
    client_add_event(1, ClientDict, {"Client 1", "caller", FutureDateTime}),
    client_add_event(2, ClientDict, {"Client 2", "callee", FutureDateTime}),
    client_cancel_event(1, ClientDict),
    client_cancel_event(2, ClientDict).
 %% ok = add_event("User B", "receives call", NextMinuteDataTime).








add_subscriber(0, ClientDict) -> ClientDict;
add_subscriber(N, ClientDict) when N>0 ->
    Pid = spawn(fun() ->

			Result = subscribe(self())
		end),
    NewClientDict = orddict:store(N, Pid, ClientDict),
    add_subscriber(N-1, NewClientDict).

client_cancel_event(Client, Dict) ->
    ClientPid = orddict:fetch(Client, Dict),
    Ref = make_ref(),
    ClientPid ! {self(), Ref, cancel_event},
    receive
	{Ref, cancel_event_success} ->
	    {ClientPid, cancel_event_success}
    end.

client_add_event(Client, Dict, {EventName, EventDescription, EventTimeOut}) ->
    ClientPid = orddict:fetch(Client, Dict),
    Ref = make_ref(),
    ClientPid ! {self(), Ref, client_add_event,
		 {EventName, EventDescription, EventTimeOut}},
    receive
	{Ref, client_added_event} ->
	    {Client, EventName, added_success};
	{Ref, client_add_event, server_error} ->
	    server_error_504
    end.



