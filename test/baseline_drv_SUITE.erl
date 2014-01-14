%% =============================================================================
%% =============================================================================

-module(baseline_drv_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [
          {group, test_public}
         ].

groups() ->
    [
     {test_public, [], [
                        {group, test_call_sequential},
                        {group, test_call_parallel}
                       ]},

     {test_call_parallel, [], [
                               call_parallel_call_test,
                               call_parallel_command_test,
                               call_parallel_control_test
                              ]},
     {test_call_sequential, [], [
                                 call_sequential_call_test,
                                 call_sequential_command_test,
                                 call_sequential_control_test
                                ]}
    ].

init_per_testcase(_TestCase, Config) ->
    L = [ fun load/1, fun start_link/1 ],
    try lists:foldl(fun(E,A) -> E(A) end, Config, L)
    catch
        Reason ->
            ct:fail(Reason)
    end.

end_per_testcase(_TestCase, Config) ->
    L = [ fun stop/1, fun unload/1 ],
    try lists:foldl(fun(E,A) -> E(A) end, Config, L)
    catch
        Reason ->
            ct:fail(Reason)
    end.

%% == group: test_call_parallel ==

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
         { [command, 0,[5,"hello",1]], {error,ebusy} },
         { [command, 0,[5,"world",1]], {error,ebusy} },
         { [command, 0,[1,"!",1]],     {error,ebusy} },
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

%% == group: test_call_sequential ==

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
         { [command, 0,[5,"hello",1]], {ok,"hello"} },
         { [command, 0,[5,"world",1]], {ok,"world"} },
         { [command, 0,[1,"!",1]],     {ok,"!"} },
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

%% == ==

execute(Function, Args) ->
    baseline_ct:execute(baseline_drv, Function, Args).

execute_parallel(Pid, Function, List) ->
    Self = self(),
    [ spawn(fun() -> Self ! E = execute(Function,[Pid|A]) end) || {A,E} <- List ],
    [ receive _ -> ok after 10000 -> ct:fail(timeout) end || _ <- List ].

execute_sequential(Pid, Function, List) ->
    [ E = execute(Function,[Pid|A]) || {A,E} <- List ].

load(Config) ->
    L = [
         {path, filename:join([baseline_ct:base_dir(2),"priv","lib"])},
         {name, "baseline_drv"}
        ],
    case baseline_port:load(L) of
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
    case baseline_drv:stop(?config(pid,Config)) of
        ok ->
            proplists:delete(pid, Config);
        {error, Reason} ->
            throw(Reason)
    end.

unload(Config) ->
    case baseline_port:unload(?config(handle,Config)) of
        ok ->
            proplists:delete(handle, Config);
        {error, Reason} ->
            throw(Reason)
    end.
