#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/baseline/ebin -config files/n1

run(0, Pid) ->
    L = [
         { 0, []},
         { 0, []},
         { 0, []},
         {99, []}
        ],
    [ io:format("call(~p)=~p~n", [C,baseline_drv:call(Pid,C,A,infinity)]) || {C,A} <- L ].

run(direct) ->
    io:format("run: direct~n"),
    case baseline_port:load([{name,"baseline_drv"}]) of
        {ok, H} ->
            L = [
                 {handle,H}
                ],
            case baseline_drv:start_link(L) of
                {ok, Pid} ->
                    run(0, Pid),
                %   exit(Pid, kill);
                    baseline_drv:stop(Pid);
                {error, Reason} ->
                    io:format("ERROR: ~p (port)~n", [Reason])
            end,
            baseline_port:unload(H);
        {error, Reason} ->
            io:format("ERROR: ~p (driver)~n", [Reason])
    end.

main(_) ->
    L = [
         direct
        ],
    [ run(A) || A <- L ].
