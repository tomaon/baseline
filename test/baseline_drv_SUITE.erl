%% =============================================================================
%% =============================================================================

-module(baseline_drv_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% -- pubic --
-export([
         start_link_test/1,
         call_sequential_test/1, call_parallel_test/1,
         command_sequential_test/1, command_parallel_test/1,
         control_sequential_test/1, control_parallel_test/1,
         handle_call_test/1,
         handle_cast_test/1,
         handle_info_data_test/1, handle_info_exit_test/1, handle_info_ping_test/1
        ]).

%% == callback: ct ==

all() -> [
          start_link_test,
          call_sequential_test, call_parallel_test,
          command_sequential_test, command_parallel_test,
          control_sequential_test, control_parallel_test,
          handle_call_test,
          handle_cast_test,
          handle_info_data_test, handle_info_exit_test, handle_info_ping_test
         ].

init_per_suite(Config) ->
    L  = [
          {path, filename:join([baseline_ct:base_dir(2),"priv","lib"])},
          {name, <<"baseline_drv">>}
         ],
    baseline_ct:loop(skip, [{driver,L}|Config], [fun load/1, fun unload/1]).

end_per_suite(Config) ->
    proplists:delete(driver, Config).

init_per_testcase(start_link_test, Config) ->
    baseline_ct:loop(fail, Config, [fun load/1]);
init_per_testcase(_TestCase, Config) ->
    baseline_ct:loop(fail, Config, [fun load/1, fun start_link/1]).

end_per_testcase(start_link_test, Config) ->
    baseline_ct:loop(fail, Config, [fun unload/1]);
end_per_testcase(_TestCase, Config) ->
    baseline_ct:loop(fail, Config, [fun stop/1, fun unload/1]).

%% == public ==

start_link_test(_Config) ->
    X = [
         { ["undefined",[]],     {error,badarg} },
         { [<<"undefined">>,[]], {error,badarg} }
        ],
    [ E = test(start_link,A) || {A,E} <- X ].


call_sequential_test(Config) ->
    X = [
         { [call, 0,[]],                   ok },
         { [call, 0,[{encoding,"utf16"}]], ok },
         { [call, 0,[{maxlength,64}]],     {error,{badarg,maxlength}} },
         { [call,99,[]],                   {error,enosys} }
        ],
    test_sequential(?config(pid,Config), call, X).

call_parallel_test(Config) ->
    X = [
         { [call, 0,[]],                   ok },
         { [call, 0,[{encoding,"utf16"}]], ok },
         { [call, 0,[{maxlength,64}]],     {error,{badarg,maxlength}} },
         { [call,99,[]],                   {error,enosys} }
        ],
    test_parallel(?config(pid,Config), call, X).


command_sequential_test(Config) ->
    X = [
         { [command, 0,[0,"",1]],      ok },
         { [command, 0,[5,"hello",0]], {ok,"hello"} },
         { [command, 0,[5,"world",0]], {ok,"world"} },
         { [command, 0,[1,"!",0]],     {ok,"!"} },
         { [command,99,[]],            {error,enosys} }
        ],
    test_sequential(?config(pid,Config), call, X).

command_parallel_test(Config) ->
    X = [
         { [command, 0,[0,"",1]],      ok },
         { [command, 0,[5,"hello",0]], {error,ebusy} },
         { [command, 0,[5,"world",0]], {error,ebusy} },
         { [command, 0,[1,"!",0]],     {error,ebusy} },
         { [command,99,[]],            {error,ebusy} }
        ],
    test_parallel(?config(pid,Config), call, X).


control_sequential_test(Config) ->
    X = [
         { [control, 0,[]],                   ok },
         { [control, 0,[{encoding,"utf16"}]], ok },
         { [control, 0,[{maxlength,64}]],     {error,{badarg,maxlength}} },
         { [control,99,[]],                   {error,enosys} }
        ],
    test_sequential(?config(pid,Config), call, X).

control_parallel_test(Config) ->
    X = [
         { [control, 0,[]],                   ok },
         { [control, 0,[{encoding,"utf16"}]], ok },
         { [control, 0,[{maxlength,64}]],     {error,{badarg,maxlength}} },
         { [control,99,[]],                   {error,enosys} }
        ],
    test_parallel(?config(pid,Config), call, X).


handle_call_test(Config) ->
    X = [
         { [[ping]], {error,badarg} }
        ],
    [ E = apply(gen_server,call,[?config(pid,Config)|A]) || {A,E} <- X ].


handle_cast_test(Config) ->
    X = [
         { [[ping]], ok }
        ],
    [ E = apply(gen_server,cast,[?config(pid,Config)|A]) || {A,E} <- X ].


handle_info_data_test(Config) ->
    P = baseline_drv:find(?config(name,Config)),
    ?config(pid,Config) ! {P,{data,<<>>}},
    receive _ -> ok  after 1000 -> ok end,
    false =:= is_process_alive(?config(pid,Config)).

handle_info_exit_test(Config) ->
    spawn_link(fun() -> link(?config(pid,Config)) end),
    receive _ -> ok  after 1000 -> ok end,
    false =:= is_process_alive(?config(pid,Config)).

handle_info_ping_test(Config) ->
    ?config(pid,Config) ! ping,
    receive _ -> ct:fail(ebadmsg) after 1000 -> ok end.

%% == private ==

test(Function, Args) ->
    baseline_ct:test(baseline_drv, Function, Args).

test_parallel(Pid, Function, List) ->
    L = lists:map(fun({A,E}) -> {[Pid|A],E} end, List),
    baseline_ct:test_parallel(baseline_drv, Function, L).

test_sequential(Pid, Function, List) ->
    L = lists:map(fun({A,E}) -> {[Pid|A],E} end, List),
    baseline_ct:test_sequential(baseline_drv, Function, L).

%% -- --

load(Config) ->
    L = ?config(driver, Config),
    case baseline_drv:load(L) of
        ok ->
            [{name,proplists:get_value(name,L)}|Config];
        {error, Reason} ->
            throw(Reason)
    end.

start_link(Config) ->
    L = [
         ?config(name,Config),
         []
        ],
    case baseline_drv:start_link(L) of
        {ok, Pid} ->
            unlink(Pid),
            [{pid,Pid}|Config];
        {error, Reason} ->
            throw(Reason)
    end.

stop(Config) ->
    stop(Config, proplists:is_defined(pid,Config)).

stop(Config, true) ->
    case is_process_alive(?config(pid,Config)) of
        true ->
            case baseline_drv:stop(?config(pid,Config)) of
                ok ->
                    proplists:delete(pid, Config);
                {error, Reason} ->
                    throw(Reason)
            end;
        false ->
            Config
    end;
stop(Config, false) ->
    Config.

unload(Config) ->
    case baseline_drv:unload(?config(name,Config)) of
        ok ->
            proplists:delete(name, Config);
        {error, Reason} ->
            throw(Reason)
    end.
