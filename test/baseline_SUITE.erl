%% =============================================================================
%% =============================================================================

-module(baseline_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).

%% -- public --
-export([
         flush_test/1
        ]).

%% == callback: ct ==

all() -> [
          flush_test
         ].

%% == public ==

flush_test(_Config) ->
    [ self() ! ping || _ <- lists:seq(1,10) ],
    ok = test(flush, []).

%% == private ==

test(Function, Args) ->
    baseline_ct:test(baseline, Function, Args).
