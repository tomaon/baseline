#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/baseline/ebin -config files/n1

run(0,P) ->
    X = [
         { [command, 0,[5,"hello",0]], {ok,"hello"} }
        ],
    [ E = apply(baseline_drv,call,[P|A]) || {A,E} <- X ];
run(_,_) ->
    ok.

run(baseline_drv) ->
    io:format("run: baseline_drv~n"),
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
    end;
run(_) ->
    ok.

main(_) ->
    L = [
         baseline_drv
        ],
    [ run(A) || A <- L ].
