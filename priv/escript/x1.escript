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

run(app) ->
    io:format("run: app~n"),
    N = baseline_drv,
    case application:start(N) of
        ok ->
            application:stop(N);
        {error, Reason} ->
            io:format("ERROR: ~p (app)~n", [Reason])
    end;
run(direct) ->
    io:format("run: direct~n"),
    N = <<"baseline_drv">>,
    L = [
         {application,binary_to_atom(N,latin1)},
         {name,N}
        ],
    case baseline_drv:load(L) of
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
run(dir) ->
    io:format("run: dir~n"),
    R = code:root_dir(),
    io:format("~p~n", [[ E || E <- code:get_path(), not lists:prefix(R,E) ]]);
run(_) ->
    ok.

main(_) ->
    L = [
         dir,
         app,
         direct
        ],
    [ run(A) || A <- L ].
