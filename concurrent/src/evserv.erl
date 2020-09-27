%% Event server
-module(evserv).
-compile(export_all).

-define(CLIENT_TIMEOUT, 1000*60*5).  % 5 minutes
-define(SERVER_TIMEOUT, 1000*60*15). % 15 minutes

-record(state, {events,    %% list of #event{} records
                clients}). %% list of Pids

-record(event, {name="",
                description="",
                pid,
                timeout={{1970,1,1},{0,0,0}}}).

%%% User Interface

start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

init() ->
    %% Loading events from a static file could be done here.
    %% You would need to pass an argument to init (maybe change the functions
    %% start/0 and start_link/0 to start/1 and start_link/1) telling where the
    %% resource to find the events is. Then load it from here.
    %% Another option is to just pass the event straight to the server
    %% through this function.
    loop(#state{events=orddict:new(),
                clients=orddict:new()}).

subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    subscribe_helper().


subscribe_helper() ->
    receive
	{Ref, ok} ->
	    io:format("Client ~p subscribe successfully ~p~n", [self(), Ref]),
	    subscribe_helper();
	{event_timeout, EventName, _EventDescription} ->
	    io:format("Client ~p receives ~p event timeout ~n", [self(), EventName]),
	    subscribe_helper();
	{'DOWN', _Ref, process, _Pid, Reason} ->
	    {client, self(), server_down, Reason};
	{Pid, Ref, client_add_event, {EventName, EventDescription, EventTimeOut}} ->
	    
	    Result =  add_event(EventName, EventDescription, EventTimeOut),
	    case Result of
		{server_error, timeout} ->
		    Pid ! {Ref, client_add_event, server_error};
		Msg ->
		    Pid ! {Ref, client_added_event},
		    subscribe_helper()
	   end;
	{Pid, Ref, cancel_event} ->
	    cancel(),
	    Pid ! {Ref, cancel_event_success}
	    

    after ?CLIENT_TIMEOUT -> % timeout 10 minutes
	 io:format("Client ~p can't connect to server~n", [self()])
    end.



add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
        {server_error, timeout}
    end.

add_event2(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, {error, Reason}} -> erlang:error(Reason);
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

cancel() ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, cancel_event},
    receive
        {Ref, ok} -> ok
    after 5000 ->
        {error, timeout}
    end.

listen(Delay) ->
    receive
        M = {done, _Name, _Description} ->
            [M | listen(0)]
    after Delay*1000 ->
        []
    end.

%%% The Server itself

loop(S=#state{}) ->
    receive
        {Pid, MsgRef, {subscribe, ClientPid}} ->
            erlang:monitor(process, ClientPid),
            NewClients = orddict:store(MsgRef, ClientPid, S#state.clients),
            Pid ! {MsgRef, ok},
            loop(S#state{clients=NewClients});


        {ClientPid, MsgRef, {add, Name, Description, TimeOut}} ->
            case valid_datetime(TimeOut) of
                true ->
                    EventPid = event:start_link(ClientPid, Name, TimeOut),
                    NewEvents = orddict:store(ClientPid,
					      #event{name=Name,
						     description=Description,
						     pid=EventPid,
						     timeout=TimeOut},
					       S#state.events),
                    ClientPid ! {MsgRef, ok},
                    loop(S#state{events=NewEvents});
                false ->
                    ClientPid ! {MsgRef, {error, bad_timeout}},
                    loop(S)
            end;
        
	{ClientPid, MsgRef, cancel_event} ->
            Events = case orddict:find(ClientPid, S#state.events) of
                         {ok, E} ->
                             event:cancel(E#event.pid),
                             orddict:erase(ClientPid, S#state.events);
                         error ->
                             S#state.events
                     end,
            ClientPid ! {MsgRef, ok},
            loop(S#state{events=Events});
        {event_timeout, MasterPid, Name} ->
            case orddict:find(MasterPid, S#state.events) of
                {ok, E} ->
		    MasterPid ! {event_timeout, E#event.name, E#event.description},

                    NewEvents = orddict:erase(MasterPid, S#state.events),
                    loop(S#state{events=NewEvents});
                error ->
                    %% This may happen if we cancel an event and
                    %% it fires at the same time
                    loop(S)
            end;
        shutdown ->
            exit(shutdown);
        {'DOWN', Ref, process, Pid, Reason} ->
	    io:format("Process: ~p down Reason: ~p~n", [Pid, Reason]),
            loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
        code_change ->
            ?MODULE:loop(S);
        {Pid, debug} -> %% used as a hack to let me do some unit testing
            Pid ! S,
            loop(S);
        Unknown ->
            io:format("Unknown message: ~p~n",[Unknown]),
            loop(S)
    after ?SERVER_TIMEOUT ->
	    server_out_of_service
	      
    end.


%%% Internal Functions
send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

valid_datetime({Date,Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause -> %% not in {{Y,M,D},{H,Min,S}} format
            false
    end;
valid_datetime(_) ->
    false.

%% calendar has valid_date, but nothing for days.
%% This function is based on its interface.
%% Ugly, but ugh.
valid_time({H,M,S}) -> valid_time(H,M,S).

valid_time(H,M,S) when H >= 0, H < 24,
                       M >= 0, M < 60,
                       S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.
