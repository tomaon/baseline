#!/usr/bin/env escript
%% -*- erlang -*-

tests(Dir) ->
    [ {all,filename:join([Dir,E])} || E <- lists:reverse(filelib:wildcard("ct_run.*",Dir)) ].

main([Dir]) ->
    ct_cover:cross_cover_analyse(details, tests(Dir)).
