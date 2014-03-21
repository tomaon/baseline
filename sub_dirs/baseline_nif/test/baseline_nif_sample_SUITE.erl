%% =============================================================================
%% =============================================================================

-module(baseline_nif_sample_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% -- pubic --
-export([
         init_test/1,
         option_int_test/1, option_uint_test/1,
         option_long_test/1, option_ulong_test/1,
         option_bool_test/1,
         option_atom_test/1,
         option_string_test/1
        ]).

%% == callback: ct ==

all() -> [
          init_test,
          option_int_test, option_uint_test,
          option_long_test, option_ulong_test,
          option_bool_test,
          option_atom_test,
          option_string_test
         ].

init_per_testcase(_TestCase, Config) ->
    baseline_ct:loop(fail, Config, [fun new/1]).

end_per_testcase(_TestCase, Config) ->
    baseline_ct:loop(fail, Config, [fun delete/1]).

%% == public ==

init_test(Config) ->
    X = [
         { get, [], {ok,{0,0,0,0,0,'',[]}} }
        ],
    [ E = test(F,[?config(resource,Config)|A]) || {F,A,E} <- X ].


option_int_test(Config) ->
    K = 'i',
    X = [
         { set, [[{K,2147483647}]], ok },
         { get, [], {ok,{2147483647,0,0,0,0,'',[]}} },
         { set, [[{K,0}]], ok },
         { get, [], {ok,{0,0,0,0,0,'',[]}} },
         { set, [[{K,-2147483647}]], ok },
         { get, [], {ok,{-2147483647,0,0,0,0,'',[]}} }
        ],
    [ E = test(F,[?config(resource,Config)|A]) || {F,A,E} <- X ].

option_uint_test(Config) ->
    K = 'ui',
    X = [
         { set, [[{K,4294967295}]], ok },
         { get, [], {ok,{0,4294967295,0,0,0,'',[]}} },
         { set, [[{K,0}]], ok },
         { get, [], {ok,{0,0,0,0,0,'',[]}} }
        ],
    [ E = test(F,[?config(resource,Config)|A]) || {F,A,E} <- X ].


option_long_test(Config) ->
    K = 'l',
    X = [
         { set, [[{K,9223372036854775807}]], ok },
         { get, [], {ok,{0,0,9223372036854775807,0,0,'',[]}} },
         { set, [[{K,0}]], ok },
         { get, [], {ok,{0,0,0,0,0,'',[]}} },
         { set, [[{K,-9223372036854775807}]], ok },
         { get, [], {ok,{0,0,-9223372036854775807,0,0,'',[]}} }
        ],
    [ E = test(F,[?config(resource,Config)|A]) || {F,A,E} <- X ].

option_ulong_test(Config) ->
    K = 'ul',
    X = [
         { set, [[{K,18446744073709551615}]], ok },
         { get, [], {ok,{0,0,0,18446744073709551615,0,'',[]}} },
         { set, [[{K,0}]], ok },
         { get, [], {ok,{0,0,0,0,0,'',[]}} }
        ],
    [ E = test(F,[?config(resource,Config)|A]) || {F,A,E} <- X ].


option_bool_test(Config) ->
    K = 'b',
    X = [
         { set, [[{K,true}]], ok },
         { get, [], {ok,{0,0,0,0,1,'',[]}} },
         { set, [[{K,false}]], ok },
         { get, [], {ok,{0,0,0,0,0,'',[]}} },
         { set, [[{K,"TRUE"}]], ok },
         { get, [], {ok,{0,0,0,0,1,'',[]}} },
         { set, [[{K,"FALSE"}]], ok },
         { get, [], {ok,{0,0,0,0,0,'',[]}} }
        ],
    [ E = test(F,[?config(resource,Config)|A]) || {F,A,E} <- X ].


option_atom_test(Config) ->
    K = 'a',
    X = [
         { set, [[{K,true}]], ok },
         { get, [], {ok,{0,0,0,0,0,true,[]}} },
         { set, [[{K,false}]], ok },
         { get, [], {ok,{0,0,0,0,0,false,[]}} },
         { set, [[{K,"TRUE"}]], ok },
         { get, [], {ok,{0,0,0,0,0,'TRUE',[]}} },
         { set, [[{K,"FALSE"}]], ok },
         { get, [], {ok,{0,0,0,0,0,'FALSE',[]}} }
        ],
    [ E = test(F,[?config(resource,Config)|A]) || {F,A,E} <- X ].


option_string_test(Config) ->
    K = 's',
    X = [
         { set, [[{K,true}]], ok },
         { get, [], {ok,{0,0,0,0,0,'',"true"}} },
         { set, [[{K,false}]], ok },
         { get, [], {ok,{0,0,0,0,0,'',"false"}} },
         { set, [[{K,"TRUE"}]], ok },
         { get, [], {ok,{0,0,0,0,0,'',"TRUE"}} },
         { set, [[{K,"FALSE"}]], ok },
         { get, [], {ok,{0,0,0,0,0,'',"FALSE"}} }
        ],
    [ E = test(F,[?config(resource,Config)|A]) || {F,A,E} <- X ].

%% == private ==

test(Function, Args) ->
    baseline_ct:test(baseline_nif_sample, Function, Args).

%% -- --

new(Config) ->
    case test(new, [123]) of
        {ok, R} ->
            [{resource,R}|Config];
        {error, Reason} ->
            throw(Reason)
    end.

delete(Config) ->
    case test(delete, [?config(resource,Config)]) of
        ok ->
            proplists:delete(resource, Config);
        {error, Reason} ->
            throw(Reason)
    end.
