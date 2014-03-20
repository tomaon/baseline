#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -pa deps/baseline/ebin -config priv/conf/n1

run(0,M,R) ->
    X = [
         { get, [], {ok,{0,0,0,'',[]}} },
         { set, [[{'option_1',456}]], ok },
         { get, [], {ok,{456,0,0,'',[]}} },
         { set, [[{'option_2',789}]], ok },
         { get, [], {ok,{456,789,0,'',[]}} },
         { set, [[{'option_3',true}]], ok },
         { get, [], {ok,{456,789,1,'',[]}} },
         { set, [[{'option_3',"false"}]], ok },
         { get, [], {ok,{456,789,0,'',[]}} },
         { set, [[{'option_4',true}]], ok },
         { get, [], {ok,{456,789,0,true,[]}} },
         { set, [[{'option_4',"false"}]], ok },
         { get, [], {ok,{456,789,0,false,[]}} },
         { set, [[{'option_5',"true"}]], ok },
         { get, [], {ok,{456,789,0,false,"true"}} },
         { set, [[{'option_5',false}]], ok },
         { get, [], {ok,{456,789,0,false,"false"}} }
        ],
    [ io:format("~p = ~p,~p~n", [E = apply(M,F,[R|A]),F,A]) || {F,A,E} <- X ];
run(_,_,_) ->
    ok.

run(direct) ->
    io:format("run: direct~n"),
    M = baseline_nif_sample,
    case apply(M, new, [123]) of
        {ok, R} ->
            [ run(E,M,R) || E <- [0] ],
            apply(M, delete, [R]);
        {error, Reason} ->
            io:format("ERROR: ~p (driver)~n", [Reason])
    end;
run(_) ->
    ok.

main(_) ->
    L = [
         direct
        ],
    [ run(A) || A <- L ].
