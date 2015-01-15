%% =============================================================================
%% =============================================================================

-module(baseline_binary_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).

%% -- pubic --
-export([
         prefix_test/1,
         suffix_test/1
        ]).

%% == callback: ct ==

all() -> [
          prefix_test,
          suffix_test
         ].

%% == public ==

prefix_test(_Config) ->
    X = [
         { [<<"123">>,<<"12">>], true },
         { [<<"12">>,<<"12">>], true },
         { [<<"1">>,<<"12">>], false },
         { [<<"123">>,<<"">>], false },
         { [<<"">>,<<"12">>], false },
         { [<<"">>,<<"">>], false }
        ],
    [ E = test(prefix,A) || {A,E} <- X ].

suffix_test(_Config) ->
    X = [
         { [<<"123">>,<<"23">>], true  },
         { [<<"23">>, <<"23">>], true  },
         { [<<"3">>,  <<"23">>], false },
         { [<<"123">>,<<"">>], false },
         { [<<"">>,<<"23">>], false },
         { [<<"">>,<<"">>], false }
        ],
    [ E = test(suffix,A) || {A,E} <- X ].

%% == private ==

test(Function,Args) ->
    baseline_ct:test(baseline_binary, Function, Args).
