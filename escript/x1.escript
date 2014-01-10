#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/*/ebin

run(_) ->
    ok.

main(_) ->
    L = [
        ],
    io:format("~p~n", [lists:foldl(fun(E,A) -> run(E,A) end, 0, L)]).
