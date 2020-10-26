-module(multicore).
-export([test1/0,test2/0]).


make_random_list(K) -> [random:uniform(1000000) || _ <- lists:duplicate(K, null)].

seed() -> random:seed(44,55,66).

fibo(0) -> 1;
fibo(1) -> 1;
fibo(N) -> fibo(N-1) + fibo(N-2).


test1() ->
    seed(),
    L1 = [make_random_list(1000) || _ <- lists:duplicate(100, null)],
    {Time1, S1} = timer:tc(lists, map, [fun lists:sort/1, L1]),
    {Time2, S2} = timer:tc(plists, map, [fun lists:sort/1, L1]),
    io:format("sort: results equal? = ~w, speedup = ~w~n", [S1=:=S2, Time1/Time2]).



test2() ->
    L2 = lists:duplicate(100, 27),
    {Time1, S1} = timer:tc(lists, map, [fun fibo/1, L2]),
    {Time2, S2} = timer:tc(plists, map, [fun fibo/1, L2]),
    io:format("fibo: results equal? = ~w, speedup = ~w~n", [S1=:=S2, Time1/Time2]).
