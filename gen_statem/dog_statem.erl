-module(dog_statem).

-behavior(gen_statem).

-export([wake_up/0]).

-export([set_action/1]).

-export([bark/3,
	 wag_tail/3,
	 sit/3,
	 get_dog_state/0]).

-export([init/1,
	 callback_mode/0,
	 terminate/3]).

-define(NAME, ?MODULE).
-define(BARK_TIMEOUT, 2000).
-define(WAG_TIMEOUT, 30000).

wake_up() ->
    gen_statem:start_link({local,?NAME}, ?MODULE, [], []).

set_action(Action) ->
    gen_statem:cast(?NAME, {action, Action}).

get_dog_state() ->
    gen_statem:call(?NAME, get_dog_state).

init(_) ->
    %% Adding Data later
    Data = [],
    io:format("Dog says: BARK! BARK!~n"),
    {ok, bark, Data, [ %% action()
		       {timeout, ?BARK_TIMEOUT, bark}
		     ]}.

callback_mode() ->
    state_functions.

%% callback funtion for the current sate
%% StateName(EventType, EventContent, Data) ->
%%    ... code for actions here ...
%%    {next_state, NewStateName, NewData}.
%% receive `pet` and move to next state `wag_tail`
bark(cast, {action, pet}, _Data) ->
    io:format("Dog wags its tail~n"),
    {next_state, wag_tail, _Data, [{timeout, ?WAG_TIMEOUT, wag_tail}]};
bark(cast, {action, _Event}, _Data) ->
    io:format("Dog is confused~n"),
    {keep_state_and_data, [{timeout, ?BARK_TIMEOUT, bark}]};
						% state timeout
bark(timeout, bark,  _Data) ->
    io:format("Dog says: BARK! BARK!~n"),
    {keep_state_and_data, [{timeout, ?BARK_TIMEOUT, bark}]};
						% state_callback_result
bark({call, From}, get_dog_state, _Data) ->
    {keep_state_and_data, {reply, From, bark}}.

%% receive `pet` and move to next state `sit`
wag_tail(cast, {action, pet}, _Data) ->
    io:format("Dog is sitting. Gooooood boy!~n"),
    {next_state, sit, _Data};
wag_tail(cast, {action, _Event}, _Data) ->
    io:format("Dog is confused~n"),
    {keep_state_and_data, [{timeout, ?BARK_TIMEOUT, wag_tail}]};

wag_tail(timeout, wag_tail,  _Data) ->
    io:format("Dog is confused~n"),
    {keep_state_and_data, [{timeout, ?BARK_TIMEOUT, wag_tail}]};    
						% state_callback_result
wag_tail({call, From}, get_dog_state, _Data) ->
    {keep_state_and_data, {reply, From, wait_tail}}.


sit(cast, {action, squirrel}, _Data) ->
    io:format("Dog says: BARK! BARK!~n"),
    {next_state, bark, _Data};
sit(cast, {action, _}, _Data) ->
    io:format("Dog is confused~n"),
    keep_state_and_data;
						% state_callback_result
sit({call, From}, get_dog_state, _Data) ->
    {keep_state_and_data, {reply, From, sit}}.

terminate(_Reason, _State, _Data) ->
    ok.
