%% Write an Erlang module to perform a binary search on a sorted list of numbers at least 10 values in length.
%% The module should return the position of the number if it is found or a statement that it is not contained in the list.
%% Demonstrate the module by calling it from the Erlang shell, passing the list of values to the module.
-module(binary_search).
-export([search/2]).


search(Num, List) when length(List) == 1 ->
    [Elem] = List,
    case Elem of
	 Num -> found;
	_Other -> not_found
    end;

search(Num, List)  ->
    Middle = get_middle(List),
    case Middle of
	Num ->
	    found;
	_Other ->
	    case Num < Middle of
		true ->
		    LeftSide = get_left(Middle, List),
		    case LeftSide of
			[] -> %%% (3, [4,5,6])
			    not_found;
			_ ->
			    search(Num, LeftSide)
		    end;
		false  -> %Num > Middle
		    RightSide = get_right(Middle, List),
		    case RightSide of
			[] ->  %%% (10, 6,7,8)
			    not_found;
			_ ->
			    search(Num, RightSide)
		    end
	    end
    end.

get_middle(List) ->
    Len = length(List),
    lists:nth(Len div 2, List).

get_left(Middle, List) ->
    [X || X <- List, X < Middle].
    
get_right(Middle, List) ->
    [X || X <- List, X > Middle].


		    
