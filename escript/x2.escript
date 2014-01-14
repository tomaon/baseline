#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/baseline/ebin -config files/n1

execute(Pid, Function, Type, Command, Args) ->
    apply(baseline_drv, Function, [Pid,Type,Command,Args,infinity]).

execute_parallel(P, F, L) ->
    S = self(),
    [ spawn(fun() -> S ! {T,C,execute(P,F,T,C,A)} end) || {[T,C,A]} <- L ],
    [ receive {T,C,V} -> io:format("call(~p,~p)=~p~n", [T,C,V]) end || _ <- L ].

execute_sequential(P, F, L) ->
    [ io:format("call(~p,~p)=~p~n", [T,C,execute(P,F,T,C,A)]) || {[T,C,A]} <- L ].

run(5, P) ->
    io:format("run: parallel~n"),
    L = [
         { [call, 0,[]] },
         { [call, 0,[{encoding,"utf16"}]] },
         { [call, 0,[{maxlength,64}]] },
         { [call,99,[]] }
        ],
    execute_parallel(P, call, L);
run(4, P) ->
    io:format("run: sequential~n"),
    L = [
         { [call, 0,[]] },
         { [call, 0,[{encoding,"utf16"}]] },
         { [call, 0,[{maxlength,64}]] },
         { [call,99,[]] }
        ],
    execute_sequential(P, call, L);
run(3, P) ->
    io:format("run: parallel~n"),
    L = [
         { [control, 0,[]] },
         { [control, 0,[{encoding,"utf16"}]] },
         { [control, 0,[{maxlength,64}]] },
         { [control,99,[]] }
        ],
    execute_parallel(P, call, L);
run(2, P) ->
    io:format("run: sequential~n"),
    L = [
         { [control, 0,[]] },
         { [control, 0,[{encoding,"utf16"}]] },
         { [control, 0,[{maxlength,64}]] },
         { [control,99,[]] }
        ],
    execute_sequential(P, call, L);
run(1, P) ->
    io:format("run: parallel~n"),
    L = [
         { [command, 0,[0,"",1]] },
         { [command, 0,[5,"hello",1]] },
         { [command, 0,[5,"world",1]] },
         { [command, 0,[1,"!",1]] },
         { [command,99,[]] }
        ],
    execute_parallel(P, call, L);
run(0, P) ->
    io:format("run: sequential~n"),
    L = [
         { [command, 0,[0,"",1]] },
         { [command, 0,[5,"hello",1]] },
         { [command, 0,[5,"world",1]] },
         { [command, 0,[1,"!",1]] },
         { [command,99,[]] }
        ],
    execute_sequential(P, call, L);
run(_,_) ->
    ok.

run(direct) ->
    io:format("run: direct~n"),
    case baseline_port:load([{name,"baseline_drv"}]) of
        {ok, H} ->
            L = [
                 {handle,H}
                ],
            case baseline_drv:start_link(L) of
                {ok, Pid} ->
                    [ run(E,Pid) || E <- [2,3,4,5] ],
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
