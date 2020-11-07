-module(test_ets).
-compile(export_all).

%% https://en.wikibooks.org/wiki/Erlang_Programming/Using_ets
%% 
start() -> start( mouse ).

start( Animal ) ->
	Kingdom = ets:new( 'magic',  [] ),
	% note: table is not square
	populate( Kingdom, [{micky,mouse}, {mini,mouse}, {goofy}] ),
	Member = ets:member( Kingdom, micky ),
	io:format( " member ~w ~n ", [ Member ] ),
	show_next_key( Kingdom, ets:first(Kingdom) ),
	find_animal( Kingdom, Animal ).
	
show_next_key( _Kingdom, '$end_of_table' ) -> done;
show_next_key( Kingdom,  Key) ->
	Next = ets:next( Kingdom, Key ),
	io:format( " next ~w ~n ", [ Next ] ),
	show_next_key( Kingdom, Next ).

populate( _Kingdom, [] ) -> {done,start};
populate( Kingdom, [H | T] ) ->
		ets:insert( Kingdom, H ),
		populate( Kingdom, T ).
	
find_animal( Kingdom, Animal ) ->
	ets:match( Kingdom, { '$1', Animal } ). 
