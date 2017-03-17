-module(baseline_drv_sample_SUITE).

-include_lib("common_test/include/ct.hrl").

%% -- public --
-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2]).

-export([foo_test/1, bar_test/1, baz_test/1]).

%% == public ==

all() -> [
          {group, group_public}
         ].

groups() -> [
             {group_public, [parallel], [
                                         foo_test, bar_test, baz_test
                                        ]}
            ].

init_per_suite(Config) ->
    case test(start, []) of
        ok ->
            Config;
        {error, Reason} ->
            ok = ct:fail(Reason)
    end.

end_per_suite(_Config) ->
    case test(stop, []) of
        ok ->
            ok;
        {error, Reason} ->
            ok = ct:fail(Reason)
    end.

init_per_group(GroupName, Config) ->
    case test(start_link, [GroupName]) of
        {ok, Pid} ->
            true = unlink(Pid),
            save(Config, Pid);
        {error, Reason} ->
            ok = ct:fail(Reason)
    end.

end_per_group(_GroupName, Config) ->
    case test(Config, stop, []) of
        ok ->
            ok;
        {error, Reason} ->
            ok = ct:fail(Reason)
    end.


foo_test(Config) ->
    L = [
         { [0], {ok, 1} },
         { [1], {ok, 2} },
         { [3], {ok, 4} }
        ],
    [ E = test(Config, foo, A) || {A, E} <- L ].

bar_test(Config) ->
    L = [
         { [0], {ok, 0} },
         { [1], {ok, 2} },
         { [3], {ok, 6} }
        ],
    [ E = test(Config, bar, A) || {A, E} <- L ].

baz_test(Config) ->
    L = [
         { [0], {error, enosys} },
         { [1], {error, enosys} },
         { [3], {error, enosys} }
        ],
    [ E = test(Config, baz, A) || {A, E} <- L ].

%% == internal ==

save(Config, Pid) ->
    [{pid, Pid}|Config].

test(Function, Args) ->
    baseline_ct:test(baseline_drv_sample, Function, Args).

test(Config, Function, Args) ->
    test(Function, [?config(pid, Config)|Args]).
