-module(baseline_binary_SUITE).

-include_lib("common_test/include/ct.hrl").

%% -- public --
-export([all/0, groups/0]).

-export([implode_test/1]).
-export([split_test/1]).
-export([prefix_test/1, suffix_test/1]).
-export([binary_to_words_test/1, binary_to_word_test/1,
         words_to_binary_test/1, word_to_binary_test/1]).

%% == public ==

all() -> [
          {group, group_public}
         ].

groups() -> [
             {group_public, [parallel], [
                                         implode_test,
                                         split_test,
                                         prefix_test, suffix_test,
                                         binary_to_words_test, binary_to_word_test,
                                         words_to_binary_test, word_to_binary_test
                                        ]}
            ].


implode_test(_Config) ->
    L = [
         { [fun integer_to_binary/1, [1, 2, 3], <<",">>], <<"1,2,3">>}
        ],
    [ E = test(implode, A) || {A, E} <- L ].


split_test(_Config) ->
    L = [
         { [<<"a=1,b=2">>, <<",">>], {[<<"a=1">>], <<"b=2">>} },
         { [<<"a=1,,b=2">>, <<",">>], {[<<"a=1">>], <<"b=2">>} },
         { [<<"a=1,b=2">>, <<"=">>, <<",">>], [[<<"a">>, <<"1">>], [<<"b">>, <<"2">>]] }
        ],
    [ E = test(split, A) || {A, E} <- L ].


prefix_test(_Config) ->
    L = [
         { [<<"123">>, <<"12">>], true  },
         { [<<"12">>,  <<"12">>], true  },
         { [<<"1">>,   <<"12">>], false },
         { [<<"123">>, <<"">>],   false },
         { [<<"">>,    <<"12">>], false },
         { [<<"">>,    <<"">>],   false }
        ],
    [ E = test(prefix, A) || {A, E} <- L ].

suffix_test(_Config) ->
    L = [
         { [<<"123">>, <<"23">>], true  },
         { [<<"23">>,  <<"23">>], true  },
         { [<<"3">>,   <<"23">>], false },
         { [<<"123">>, <<"">>],   false },
         { [<<"">>,    <<"23">>], false },
         { [<<"">>,    <<"">>],   false }
        ],
    [ E = test(suffix, A) || {A, E} <- L ].


binary_to_words_test(_Config) ->
    L = [
         { [<<1, 2, 3, 4, 5, 6, 7, 8>>, 0, little],    [67305985, 134678021] },
         { [<<1, 2, 3, 4, 5, 6, 7, 8>>, 0, big],       [16909060,  84281096] },
         { [<<1, 2, 3, 4, 5, 6, 7, 8>>, 4, 4, little], [134678021] },
         { [<<1, 2, 3, 4, 5, 6, 7, 8>>, 4, 4, big],    [ 84281096] }
        ],
    [ E = test(binary_to_words, A) || {A, E} <- L ].

binary_to_word_test(_Config) ->
    L = [
         { [<<1, 2, 3, 4, 5, 6, 7, 8>>, 0, little],  67305985 },
         { [<<1, 2, 3, 4, 5, 6, 7, 8>>, 0, big],     16909060 },
         { [<<1, 2, 3, 4, 5, 6, 7, 8>>, 4, little], 134678021 },
         { [<<1, 2, 3, 4, 5, 6, 7, 8>>, 4, big],     84281096 }
        ],
    [ E = test(binary_to_word, A) || {A, E} <- L ].

words_to_binary_test(_Config) ->
    L = [
         { [[67305985, 134678021], little], <<1, 2, 3, 4, 5, 6, 7, 8>> },
         { [[16909060, 84281096],  big   ], <<1, 2, 3, 4, 5, 6, 7, 8>> },
         { [[134678021], little], <<5, 6, 7, 8>> },
         { [[84281096],  big   ], <<5, 6, 7, 8>> }
        ],
    [ E = test(words_to_binary, A) || {A, E} <- L ].

word_to_binary_test(_Config) ->
    L = [
         { [67305985,  little], <<1, 2, 3, 4>> },
         { [16909060,  big],    <<1, 2, 3, 4>> },
         { [134678021, little], <<5, 6, 7, 8>> },
         { [84281096,  big],    <<5, 6, 7, 8>> }
        ],
    [ E = test(word_to_binary, A) || {A, E} <- L ].

%% == internal ==

test(Function, Args) ->
    baseline_ct:test(baseline_binary, Function, Args).
