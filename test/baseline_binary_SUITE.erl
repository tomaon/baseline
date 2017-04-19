-module(baseline_binary_SUITE).

-include_lib("common_test/include/ct.hrl").

%% -- public --
-export([all/0, groups/0]).

-export([decode_signed_test/1, decode_unsigned_test/1]).
-export([encode_signed_test/1, encode_unsigned_test/1]).
-export([fold_test/1]).
-export([implode_test/1]).
-export([split_test/1]).

%% == public ==

all() -> [
          {group, group_public}
         ].

groups() -> [
             {group_public, [parallel], [
                                         decode_signed_test, decode_unsigned_test,
                                         encode_signed_test, encode_unsigned_test,
                                         fold_test,
                                         implode_test,
                                         split_test
                                        ]}
            ].


decode_signed_test(_Config) ->
    L = [
         {[<<1, 2>>, little], 16#0201},
         {[<<1, 2>>, native], 16#0201},
         {[<<1, 2>>, big],    16#0102},
         %%
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 2, little], 16#0201},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 2, native], 16#0201},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 2, big],    16#0102},
         %%
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 7,  2, big], [16#0102, 16#0304, 16#0506]},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 6,  2, big], [16#0102, 16#0304, 16#0506]},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 5,  2, big], [16#0102, 16#0304]},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 8, 7, -2, big], [16#0607, 16#0405, 16#0203]},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 8, 6, -2, big], [16#0607, 16#0405, 16#0203]},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 8, 5, -2, big], [16#0607, 16#0405]}
        ],
    [ E = test(decode_signed, A) || {A, E} <- L ].

decode_unsigned_test(_Config) ->
    L = [
         {[<<1, 2>>, little], 16#0201},
         {[<<1, 2>>, native], 16#0201},
         {[<<1, 2>>, big],    16#0102},
         %%
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 2, little], 16#0201},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 2, native], 16#0201},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 2, big],    16#0102},
         %%
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 7,  2, big], [16#0102, 16#0304, 16#0506]},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 6,  2, big], [16#0102, 16#0304, 16#0506]},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 1, 5,  2, big], [16#0102, 16#0304]},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 8, 7, -2, big], [16#0607, 16#0405, 16#0203]},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 8, 6, -2, big], [16#0607, 16#0405, 16#0203]},
         {[<<9, 1, 2, 3, 4, 5, 6, 7, 8>>, 8, 5, -2, big], [16#0607, 16#0405]}
        ],
    [ E = test(decode_unsigned, A) || {A, E} <- L ].


encode_signed_test(_Config) ->
    L = [
         {[1, 2, little], <<1, 0>>},
         {[1, 2, native], <<1, 0>>},
         {[1, 2, big],    <<0, 1>>}
        ],
    [ E = test(encode_signed, A) || {A, E} <- L ].

encode_unsigned_test(_Config) ->
    L = [
         {[1, 2, little], <<1, 0>>},
         {[1, 2, native], <<1, 0>>},
         {[1, 2, big],    <<0, 1>>}
        ],
    [ E = test(encode_unsigned, A) || {A, E} <- L ].


fold_test(_Config) ->
    F = fun(E, A) -> A + binary_to_integer(E) end,
    L = [
         {[F, 0, <<"1", "2", "3", "4", "5", "6">>, 0, 6, 1],  21},
         {[F, 0, <<"1", "2", "3", "4", "5", "6">>, 0, 6, 2], 102},
         {[F, 0, <<"1", "2", "3", "4", "5", "6">>, 0, 6, 3], 579}
        ],
    [ E = test(fold, A) || {A, E} <- L ].


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
