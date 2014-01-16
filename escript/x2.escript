#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/baseline/ebin -config files/n1

run(_,_) ->
    ok.

run(direct) ->
    io:format("run: direct~n"),
    case baseline_drv:load([{name,"baseline_drv"}]) of
        {ok, H} ->
            case baseline_drv:start_link(H) of
                {ok, Pid} ->
                    unlink(Pid),
                    [ run(E,Pid) || E <- [0] ],
                %   exit(Pid, kill);
                    baseline_drv:stop(Pid);
                {error, Reason} ->
                    io:format("ERROR: ~p (port)~n", [Reason])
            end,
            baseline_drv:unload(H);
        {error, Reason} ->
            io:format("ERROR: ~p (driver)~n", [Reason])
    end.

main(_) ->
    L = [
         direct
        ],
    [ run(A) || A <- L ].
