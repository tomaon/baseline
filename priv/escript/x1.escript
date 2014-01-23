#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/baseline/ebin -config priv/conf/n1

run(0,P) ->
    X = [
         { [command, 0,[5,"hello",0]], {ok,"hello"} }
        ],
    io:format("~p~n", [ E = apply(baseline_drv,call,[P|A]) || {A,E} <- X ]);
run(_,_) ->
    ok.

run(baseline_drv) ->
    io:format("run: baseline_drv~n"),
    N = <<"baseline_drv">>,
    case baseline_drv:load([{name,N}]) of
        ok ->
            case baseline_drv:start_link(N, []) of
                {ok, Pid} ->
                    unlink(Pid),
                    [ run(E,Pid) || E <- [0] ],
                    %%exit(Pid, kill);
                    baseline_drv:stop(Pid);
                {error, Reason} ->
                    io:format("ERROR: ~p (port)~n", [Reason])
            end,
            baseline_drv:unload(N);
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
