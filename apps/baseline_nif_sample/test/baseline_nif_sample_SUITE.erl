-module(baseline_nif_sample_SUITE).

-include_lib("common_test/include/ct.hrl").

%% -- public --
-export([all/0, groups/0]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([
         init_test/1,
         option_int_test/1, option_uint_test/1,
         option_long_test/1, option_ulong_test/1,
         option_bool_test/1,
         option_atom_test/1,
         option_string_test/1
        ]).

%% == public ==

all() -> [
          {group, group_public}
         ].

groups() -> [
             {group_public, [sequence], [
                                         init_test,
                                         option_int_test, option_uint_test,
                                         option_long_test, option_ulong_test,
                                         option_bool_test,
                                         option_atom_test,
                                         option_string_test
                                        ]}
            ].

init_per_testcase(_TestCase, Config) ->
    case test(new, [123]) of
        {ok, Resource} ->
            save(Config, Resource);
        {error, Reason} ->
            ok = ct:fail(Reason)
    end.

end_per_testcase(_TestCase, Config) ->
    case test(Config, delete, []) of
        ok ->
            ok;
        {error, Reason} ->
            ok = ct:fail(Reason)
    end.


init_test(Config) ->
    L = [
         { get, [], {ok, {0, 0, 0, 0, 0, '', []}} }
        ],
    [ E = test(Config, F, A) || {F, A, E} <- L ].

option_int_test(Config) ->
    K = 'i',
    L = [
         { set, [[{K, 2147483647}]], ok },
         { get, [], {ok, {2147483647, 0, 0, 0, 0, '', []}} },
         { set, [[{K, 0}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, '', []}} },
         { set, [[{K, -2147483647}]], ok },
         { get, [], {ok, {-2147483647, 0, 0, 0, 0, '', []}} }
        ],
    [ E = test(Config, F, A) || {F, A, E} <- L ].

option_uint_test(Config) ->
    K = 'ui',
    L = [
         { set, [[{K, 4294967295}]], ok },
         { get, [], {ok, {0, 4294967295, 0, 0, 0, '', []}} },
         { set, [[{K, 0}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, '', []}} }
        ],
    [ E = test(Config, F, A) || {F, A, E} <- L ].

option_long_test(Config) ->
    K = 'l',
    L = [
         { set, [[{K, 9223372036854775807}]], ok },
         { get, [], {ok, {0, 0, 9223372036854775807, 0, 0, '', []}} },
         { set, [[{K, 0}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, '', []}} },
         { set, [[{K, -9223372036854775807}]], ok },
         { get, [], {ok, {0, 0, -9223372036854775807, 0, 0, '', []}} }
        ],
    [ E = test(Config, F, A) || {F, A, E} <- L ].

option_ulong_test(Config) ->
    K = 'ul',
    L = [
         { set, [[{K, 18446744073709551615}]], ok },
         { get, [], {ok, {0, 0, 0, 18446744073709551615, 0, '', []}} },
         { set, [[{K, 0}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, '', []}} }
        ],
    [ E = test(Config, F, A) || {F, A, E} <- L ].

option_bool_test(Config) ->
    K = 'b',
    L = [
         { set, [[{K, true}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 1, '', []}} },
         { set, [[{K, false}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, '', []}} },
         { set, [[{K, "TRUE"}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 1, '', []}} },
         { set, [[{K, "FALSE"}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, '', []}} }
        ],
    [ E = test(Config, F, A) || {F, A, E} <- L ].

option_atom_test(Config) ->
    K = 'a',
    L = [
         { set, [[{K, true}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, true, []}} },
         { set, [[{K, false}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, false, []}} },
         { set, [[{K, "TRUE"}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, 'TRUE', []}} },
         { set, [[{K, "FALSE"}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, 'FALSE', []}} }
        ],
    [ E = test(Config, F, A) || {F, A, E} <- L ].

option_string_test(Config) ->
    K = 's',
    L = [
         { set, [[{K, true}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, '', "true"}} },
         { set, [[{K, false}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, '', "false"}} },
         { set, [[{K, "TRUE"}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, '', "TRUE"}} },
         { set, [[{K, "FALSE"}]], ok },
         { get, [], {ok, {0, 0, 0, 0, 0, '', "FALSE"}} }
        ],
    [ E = test(Config, F, A) || {F, A, E} <- L ].

%% == internal ==

save(Config, Resource) ->
    [{resource, Resource}|Config].

test(Function, Args) ->
    baseline_ct:test(baseline_nif_sample, Function, Args).

test(Config, Function, Args) ->
    test(Function, [?config(resource, Config)|Args]).
