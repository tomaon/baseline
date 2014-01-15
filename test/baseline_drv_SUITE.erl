%% =============================================================================
%% =============================================================================

-module(baseline_drv_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [
          {group, group_public}
         ].

groups() ->
    [
     {group_public, [], [
                         {group, group_start_link},
                         {group, group_call_sequential},
                         {group, group_call_parallel},
                         {group, group_other}
                        ]},

     {group_call_parallel, [], [
                                call_parallel_call_test,
                                call_parallel_command_test,
                                call_parallel_control_test
                               ]},
     {group_call_sequential, [], [
                                  call_sequential_call_test,
                                  call_sequential_command_test,
                                  call_sequential_control_test
                                 ]},
     {group_start_link, [], [
                             start_link_test
                            ]},
     {group_other, [], [
                        handle_call_ping_test,
                        handle_cast_ping_test,
                        handle_info_data_test,
                        handle_info_exit_test,
                        handle_info_ping_test
                       ]}
    ].

init_per_suite(Config) ->
    L  = [
          {path, filename:join([baseline_ct:base_dir(2),"priv","lib"])},
          {name, "baseline_drv"}
         ],
    baseline_ct:loop(skip, [{driver,L}|Config], [fun load/1, fun unload/1]).

end_per_suite(_Group, Config) ->
    proplists:delete(driver, Config).

init_per_testcase(start_link_test, Config) ->
    baseline_ct:loop(fail, Config, [fun load/1]);
init_per_testcase(_TestCase, Config) ->
    baseline_ct:loop(fail, Config, [fun load/1, fun start_link/1]).

end_per_testcase(_TestCase, Config) ->
    baseline_ct:loop(fail, Config, [fun stop/1, fun unload/1]).

%% == group: group_call_parallel ==

call_parallel_call_test(Config) ->
    X = [
         { [call, 0,[]],                   ok },
         { [call, 0,[{encoding,"utf16"}]], ok },
         { [call, 0,[{maxlength,64}]],     {error,{badarg,"maxlength"}} },
         { [call,99,[]],                   {error,enosys} }
        ],
    execute_parallel(?config(pid,Config), call, X).

call_parallel_command_test(Config) ->
    X = [
         { [command, 0,[0,"",1]],      ok },
         { [command, 0,[5,"hello",0]], {error,ebusy} },
         { [command, 0,[5,"world",0]], {error,ebusy} },
         { [command, 0,[1,"!",0]],     {error,ebusy} },
         { [command,99,[]],            {error,ebusy} }
        ],
    execute_parallel(?config(pid,Config), call, X).

call_parallel_control_test(Config) ->
    X = [
         { [control, 0,[]],                   ok },
         { [control, 0,[{encoding,"utf16"}]], ok },
         { [control, 0,[{maxlength,64}]],     {error,{badarg,"maxlength"}} },
         { [control,99,[]],                   {error,enosys} }
        ],
    execute_parallel(?config(pid,Config), call, X).

%% == group: group_call_sequential ==

call_sequential_call_test(Config) ->
    X = [
         { [call, 0,[]],                   ok },
         { [call, 0,[{encoding,"utf16"}]], ok },
         { [call, 0,[{maxlength,64}]],     {error,{badarg,"maxlength"}} },
         { [call,99,[]],                   {error,enosys} }
        ],
    execute_sequential(?config(pid,Config), call, X).

call_sequential_command_test(Config) ->
    X = [
         { [command, 0,[0,"",1]],      ok },
         { [command, 0,[5,"hello",0]], {ok,"hello"} },
         { [command, 0,[5,"world",0]], {ok,"world"} },
         { [command, 0,[1,"!",0]],     {ok,"!"} },
         { [command,99,[]],            {error,enosys} }
        ],
    execute_sequential(?config(pid,Config), call, X).

call_sequential_control_test(Config) ->
    X = [
         { [control, 0,[]],                   ok },
         { [control, 0,[{encoding,"utf16"}]], ok },
         { [control, 0,[{maxlength,64}]],     {error,{badarg,"maxlength"}} },
         { [control,99,[]],                   {error,enosys} }
        ],
    execute_sequential(?config(pid,Config), call, X).

%% == group: group_start_link ==

start_link_test(_Config) ->
    X = [
         { [[]],                                         {error,badarg} },
         { [[handle]],                                   {error,badarg} },
         { [[{handle,[]}]],                              {error,{badarg,handle}} },
         { [[{handle,{}}]],                              {error,{badarg,handle}} },
         { [[{handle,{handle,undefined,undefined,[]}}]], {error,{badarg,handle}} },
         { [[{handle,{handle,undefined,name,[]}}]],      {error,badarg} },
         { [[undefined]],                                {error,badarg} }
        ],
    [ E = execute(start_link,A) || {A,E} <- X ].

%% == group: group_other ==

handle_call_ping_test(Config) ->
    X = [
         { [[ping]], {error,badarg} }
        ],
    [ E = apply(gen_server,call,[?config(pid,Config)|A]) || {A,E} <- X ].

handle_cast_ping_test(Config) ->
    X = [
         { [[ping]], ok }
        ],
    [ E = apply(gen_server,cast,[?config(pid,Config)|A]) || {A,E} <- X ].

handle_info_data_test(Config) ->
    P = baseline_port:find(baseline_port:name(?config(handle,Config))),
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

%% == ==

execute(Function, Args) ->
    baseline_ct:execute(baseline_drv, Function, Args).

execute_parallel(Pid, Function, List) ->
    L = lists:map(fun({A,E}) -> {[Pid|A],E} end, List),
    baseline_ct:execute_parallel(baseline_drv, Function, L).

execute_sequential(Pid, Function, List) ->
    L = lists:map(fun({A,E}) -> {[Pid|A],E} end, List),
    baseline_ct:execute_sequential(baseline_drv, Function, L).

load(Config) ->
    case baseline_drv:load(?config(driver,Config)) of
        {ok, H} ->
            [{handle,H}|Config];
        {error, Reason} ->
            throw(Reason)
    end.

start_link(Config) ->
    L = [
         {handle, ?config(handle,Config)}
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
    case baseline_drv:unload(?config(handle,Config)) of
        ok ->
            proplists:delete(handle, Config);
        {error, Reason} ->
            throw(Reason)
    end.
