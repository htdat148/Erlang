-module(board).

-export([init/0,
	 begin_read/0,
	 end_read/0,
	 begin_write/0, end_write/0]).

-compile([{noward_unused_function, [{board, 2}]}]).

%% opearation on board with default name
init() ->
    init(board).

begin_read() ->
    begin_read(board).

end_read() ->
    end_read(board).

begin_write() ->
    begin_write(board).

end_write() ->
    end_write(board).

%% initialize empty board and register with Name
init(Name) ->
    register(Name, spawn(fun empty_board/0)).

%% get read access to board
begin_read(Board) ->
    Ref = make_ref(),
    Board ! {begin_read, self(), Ref},
    receive
	{ok_to_read, Ref} ->
	    ok_to_read
    end.

%% release read access to board
end_read(Board) ->
    Ref = make_ref(),
    Board ! {end_read, self(), Ref}.


%% get write access to board
begin_write(Board) ->
    Ref = make_ref(),
    Board ! {begin_write, self(), Ref},
    receive
	{ok_to_write, Ref} ->
	    ok_to_write
    end.

%% release write access to board
end_write(Board) ->
    Ref = make_ref(),
    Board ! {end_write, self(), Ref}.

% board with no readers and no writers
empty_board() ->
   receive
     % serve read request
     {begin_read, From, Ref} ->
        From ! {ok_to_read, Ref}, % notify reader
        readers_board(1);         % board has one reader
     % serve write request synchronously
     {begin_write, From, Ref} ->
        From ! {ok_to_write, Ref}, % notify writer
        receive                    % wait for writer to finish
          {end_write, _From, _Ref} -> 
            empty_board()          % board is empty again
        end
   end.

% board with no readers (and no writers)
readers_board(0) -> empty_board();

% board with `Readers' active readers (and no writers)
readers_board(Readers) ->
   receive
     % serve write request
     {begin_write, From, Ref} ->
       % wait until all `Readers' have finished
       [receive {end_read, _From, _Ref} -> end_read end
          || _ <- lists:seq(1, Readers)],
       From ! {ok_to_write, Ref}, % notify writer
       receive                    % wait for writer to finish
         {end_write, _From, _Ref} -> empty_board()
       end;                       % board is empty again
     % serve read request
     {begin_read, From, Ref} ->
       From ! {ok_to_read, Ref},  % notify reader
       readers_board(Readers+1);  % board has one more reader
     % serve end read
     {end_read, _From, _Ref} ->
       readers_board(Readers-1)   % board has one less reader
   end.


% board that gives priority to readers over writers (RoW)
% `Readers' active readers and `Writers' active writers
board_RoW(Readers, Writers) ->
 receive
  {begin_read, From, Ref} when Writers =:= 0 ->
     From ! {ok_to_read, Ref},
     board_RoW(Readers+1, Writers);
  {begin_write, From, Ref} when (Writers =:= 0) and (Readers =:= 0) ->
     From ! {ok_to_write, Ref},
     board_RoW(Readers, Writers+1);
  {end_read, From, Ref} ->
     From ! {ok, Ref},
     board_RoW(Readers-1, Writers);
  {end_write, From, Ref} ->
     From ! {ok, Ref},
     board_RoW(Readers, Writers-1)
 end.
    
