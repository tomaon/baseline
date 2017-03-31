-module(baseline_sample_SUITE).

-include_lib("common_test/include/ct.hrl").

%% -- public --
-export([all/0,
         init_per_suite/1, end_per_suite/1]).

-export([start_test/1]).

%% == public ==

all() -> [
          start_test
         ].

init_per_suite(Config) ->
    case ok =:= setup(env) andalso test(start, []) of
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


start_test(_Config) ->
    {state, 3} = test(sys, get_state, [baseline_app:find(baseline_sample_sup, 1)]).

%% == internal ==

setup(Key) ->
    baseline_ct:setup(Key).

test(Function, Args) ->
    test(baseline_sample, Function, Args).

test(Module, Function, Args) ->
    baseline_ct:test(Module, Function, Args).
