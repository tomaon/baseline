%% =============================================================================
%% =============================================================================

-module(baseline_binary_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).

%% -- pubic --
-export([prefix_test/1, suffix_test/1]).
-export([binary_to_word_test/1, word_to_binary_test/1]).

%% == callback: ct ==

all() -> [
          prefix_test, suffix_test,
          binary_to_word_test, word_to_binary_test
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
         { [<<"23">>,<<"23">>], true  },
         { [<<"3">>,<<"23">>], false },
         { [<<"123">>,<<"">>], false },
         { [<<"">>,<<"23">>], false },
         { [<<"">>,<<"">>], false }
        ],
    [ E = test(suffix,A) || {A,E} <- X ].


binary_to_word_test(_Config) ->
    X = [
         { [<<1,2,3,4,5,6,7,8>>,0,little],  67305985 },
         { [<<1,2,3,4,5,6,7,8>>,0,big],     16909060 },
         { [<<1,2,3,4,5,6,7,8>>,4,little], 134678021 },
         { [<<1,2,3,4,5,6,7,8>>,4,big],     84281096 }
        ],
    [ E = test(binary_to_word,A) || {A,E} <- X ].

word_to_binary_test(_Config) ->
    X = [
         { [67305985,little],  <<1,2,3,4>> },
         { [16909060,big],     <<1,2,3,4>> },
         { [134678021,little], <<5,6,7,8>> },
         { [84281096,big],     <<5,6,7,8>> }
        ],
    [ E = test(word_to_binary,A) || {A,E} <- X ].

%% == private ==

test(Function,Args) ->
    baseline_ct:test(baseline_binary, Function, Args).
