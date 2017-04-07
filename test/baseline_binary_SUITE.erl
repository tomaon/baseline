-module(baseline_binary_SUITE).

-include_lib("common_test/include/ct.hrl").

%% -- public --
-export([all/0, groups/0]).

-export([decode_unsigned_test/1, encode_unsigned_test/1]).
-export([implode_test/1]).
-export([split_test/1]).

%% == public ==

all() -> [
          {group, group_public}
         ].

groups() -> [
             {group_public, [parallel], [
                                         decode_unsigned_test, encode_unsigned_test,
                                         implode_test,
                                         split_test
                                        ]}
            ].


decode_unsigned_test(_Config) ->
    L = [
         {[<<0, 0, 0, 1>>, 0, 4],         1},
         {[<<0, 0, 0, 1>>, 0, 4, big],    1},
         {[<<0, 0, 0, 1>>, 0, 4, little], 16777216},
         %%
         {[<<0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3>>, 0, 12, big, 4], [1, 2, 3]},
         {[<<0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3>>, 4,  9, big, 4], [2, 3]},
         {[<<0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3>>, 4,  8, big, 4], [2, 3]},
         {[<<0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3>>, 4,  7, big, 4], [2]}
        ],
    [ E = test(decode_unsigned, A) || {A, E} <- L ].

encode_unsigned_test(_Config) ->
    L = [
         {[1, 4],         <<0, 0, 0, 1>>},
         {[1, 4, big],    <<0, 0, 0, 1>>},
         {[1, 4, little], <<1, 0, 0, 0>>}
        ],
    [ E = test(encode_unsigned, A) || {A, E} <- L ].


implode_test(_Config) ->
    L = [
         { [fun integer_to_binary/1, [1, 2, 3], <<",">>], <<"1,2,3">>}
        ],
    [ E = test(implode, A) || {A, E} <- L ].


split_test(_Config) ->
    L = [
         { [<<"a=1,b=2">>, <<",">>], {[<<"a=1">>], <<"b=2">>} },
         { [<<"a=1,,b=2">>, <<",">>], {[<<"a=1">>], <<"b=2">>} }
        ],
    [ E = test(split, A) || {A, E} <- L ].

%% == internal ==

test(Function, Args) ->
    baseline_ct:test(baseline_binary, Function, Args).
