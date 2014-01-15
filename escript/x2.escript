#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/baseline/ebin -config files/n1

run(2, P) ->
    spawn_link(fun() -> link(P) end),
    timer:sleep(1000),
    V = case is_process_alive(P) of true -> error; false -> ok end,
    io:format("result: ~p~n", [V]);
run(1, P) ->
    Port = baseline_port:find("baseline_drv"),
    io:format("ports=~p~n", [Port]),
    P ! {Port,{data,<<>>}},
    receive _ -> ok  after 1000 -> ok end,
    V = case is_process_alive(P) of true -> error; false -> ok end,
    io:format("result: ~p~n", [V]);
run(0, P) ->
    X = [
         { [[ping]], ok }
        ],
    [ io:format("~p,~p ~p~n",
                [A,E,apply(gen_server,cast,[P|A])] ) || {A,E} <- X ];
run(_,_) ->
    ok.

run(direct) ->
    io:format("run: direct~n"),
    case baseline_drv:load([{name,"baseline_drv"}]) of
        {ok, H} ->
            L = [
                 {handle,H}
                ],
            case baseline_drv:start_link(L) of
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
