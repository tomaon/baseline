#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/*/ebin

run(E,_A) ->
    wait(E).

main(_) ->
    L = [
         3
        ],
    io:format("~p~n", [lists:foldl(fun(E,A) -> run(E,A) end, 0, L)]).

wait() ->
    receive timeout -> io:format("[wait] timeout!~n") end.

wait(N) ->
    timer:send_after(timer:seconds(N), timeout),
    wait().
