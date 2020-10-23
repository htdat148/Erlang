%% Write an Erlang module to perform an insertion sort for a list of numbers at least 6 values in length. The module should return the ordered list. Demonstrate the module by calling it from the Erlang shell, passing the unordered list to the module.
-module(insertion_sort).

-export([sort/1,
	 insert_sort/2]).

sort(List) ->
    sort([], List).

sort(Acc, []) -> Acc;
sort(Acc, [H|T]) ->
    sort(insert_sort(Acc, H), T).


%% Add a number to list
%% occupies correct position
insert_sort(List, N) ->
    insert_sort(List, N, []).

insert_sort([], N, Acc) ->
    Acc ++ [N];
insert_sort([H|T] = List, N, Acc) ->
    case N < H of
	true ->
	    Acc ++ [N] ++List; 
	false -> % H>N
	    insert_sort(T, N, Acc ++ [H])
    end.
	    
