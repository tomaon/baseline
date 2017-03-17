-module(baseline_lists_SUITE).

-include_lib("common_test/include/ct.hrl").

%% -- public --
-export([all/0, groups/0]).

-export([combine_test/1]).
-export([equalize_test/1]).
-export([except_test/1]).
-export([merge_test/1]).
-export([get_element_test/1]).
-export([get_value_test/1]).
-export([get_value_as_binary_test/1, get_value_as_boolean_test/1,
         get_value_as_float_test/1, get_value_as_integer_test/1,
         get_value_as_list_test/1]).

%% == public ==

all() -> [
          {group, group_public}
         ].

groups() -> [
             {group_public, [parallel], [
                                         combine_test,
                                         equalize_test,
                                         except_test,
                                         merge_test,
                                         get_element_test,
                                         get_value_test,
                                         get_value_as_binary_test, get_value_as_boolean_test,
                                         get_value_as_integer_test, get_value_as_float_test,
                                         get_value_as_list_test
                                        ]}
            ].


combine_test(_Config) ->
    L = [
         { [[{a, 1}, {b, 2}], [{b, d}, {c, e}]],      [{a, 1}, {d, 2}] },
         { [[{a, 1}, {b, 2}], [{b, d}, {c, e}], [a]], [{d, 2}] },
         { [[{a, 1}, {b, 2}], [{b, d}, {c, e}], [b]], [{a, 1}, {d, 2}] },
         { [[{a, 1}, {b, 2}], [{b, d}, {c, e}], [d]], [{a, 1}] }
        ],
    [ E = test(combine, A) || {A, E} <- L ].


equalize_test(_Config) ->
    L = [
         { [20, 1], [20] },
         { [20, 2], [10, 20] },
         { [20, 3], [7, 14, 20] },
         { [20, 4], [5, 10, 15, 20] },
         { [20, 5], [4,  8, 12, 16, 20] },
         { [20, 6], [4,  8, 11, 14, 17, 20] }
        ],
    [ E = test(equalize, A) || {A, E} <- L ].


except_test(_Config) ->
    L = [
         { [1, [],                       []],       [] },
         { [1, [],                       [{a, 9}]], [] },
         { [1, [{a, 1}, {b, 2}, {c, 3}], []],       [{a, 1}, {b, 2}, {c, 3}] },
         { [1, [{a, 1}, {b, 2}, {c, 3}], [{a, 9}]], [{b, 2}, {c, 3}] },
         { [1, [{a, 1}, {b, 2}, {c, 3}], [{b, 9}]], [{a, 1}, {c, 3}] },
         { [1, [{a, 1}, {b, 2}, {c, 3}], [{c, 9}]], [{a, 1}, {b, 2}] },
         { [1, [{a, 1}, {b, 2}, {c, 3}], [{d, 9}]], [{a, 1}, {b, 2}, {c, 3}] }
        ],
    [ E = test(except, A) || {A, E} <- L ].


merge_test(_Config) ->
    L = [
         { [1, [],                       []],       [] },
         { [1, [],                       [{a, 9}]], [{a, 9}] },
         { [1, [{a, 1}, {b, 2}, {c, 3}], []],       [{a, 1}, {b, 2}, {c, 3}] },
         { [1, [{a, 1}, {b, 2}, {c, 3}], [{a, 9}]], [{a, 9}, {b, 2}, {c, 3}] },
         { [1, [{a, 1}, {b, 2}, {c, 3}], [{b, 9}]], [{a, 1}, {b, 9}, {c, 3}] },
         { [1, [{a, 1}, {b, 2}, {c, 3}], [{c, 9}]], [{a, 1}, {b, 2}, {c, 9}] },
         { [1, [{a, 1}, {b, 2}, {c, 3}], [{d, 9}]], [{a, 1}, {b, 2}, {c, 3}, {d, 9}] }
        ],
    [ E = test(merge, A) || {A, E} <- L ].


get_element_test(_Config) ->
    L = [
         { [a, 1, [{a, 1}, {b, 2}], 2, 0 ], 1},
         { [x, 1, [{a, 1}, {b, 2}], 2, 0 ], 0}
        ],
    [ E = test(get_element, A) || {A, E} <- L ].


get_value_test(_Config) ->
    L = [
         { [a, [{a, 1}, {b, 2}]], 1},
         { [x, [{a, 1}, {b, 2}]], undefined},

         { [a, [{a, 1}, {b, 2}], 0], 1},
         { [x, [{a, 1}, {b, 2}], 0], 0}
        ],
    [ E = test(get_value, A) || {A, E} <- L ].


get_value_as_binary_test(_Config) ->
    L = [
         { [a, [{a, true}], <<>>], <<"true">> },
         { [a, [{a, 1.0}],  <<>>], <<"1.00000000000000000000e+00">> },
         { [a, [{a, 1}],    <<>>], <<"1">> },
         { [a, [{a, "1"}],  <<>>], <<"1">> },
         { [a, [],          <<>>], <<>> }
        ],
    [ E = test(get_value_as_binary, A) || {A, E} <- L ].

get_value_as_boolean_test(_Config) ->
    L = [
         { [a, [{a, true}],        false], true },
         { [a, [{a, false}],       false], false },
         { [a, [{a, <<"true">>}],  false], true },
         { [a, [{a, <<"false">>}], false], false },
         { [a, [{a, 1.0}],         false], true },
         { [a, [{a, 0.0}],         false], false },
         { [a, [{a, 1}],           false], true },
         { [a, [{a, 0}],           false], false },
         { [a, [{a, "true"}],      false], true },
         { [a, [{a, "false"}],     false], false },
         { [a, [],                 false], false }
        ],
    [ E = test(get_value_as_boolean, A) || {A, E} <- L ].

get_value_as_float_test(_Config) ->
    L = [
         { [a, [{a, <<"2.0e+0">>}], 0.0], 2.0 },
         { [a, [{a, "2.0e+0"}],     0.0], 2.0 },
         { [a, [],                  0.0], 0.0 },

         { [a, [{a, "2.0e+0"}], 0.0, 1.9], 1.9 },
         { [a, [{a, "2.0e+0"}], 0.0, 2.0], 2.0 },
         { [a, [{a, "2.0e+0"}], 0.0, 2.1], 2.0 },

         { [a, [{a, "2.0e+0"}], 0.0, 1.9, 1.9], 1.9 },
         { [a, [{a, "2.0e+0"}], 0.0, 2.0, 1.9], 2.0 },
         { [a, [{a, "2.0e+0"}], 0.0, 2.1, 2.1], 2.1 }
        ],
    [ E = test(get_value_as_float, A) || {A, E} <- L ].

get_value_as_integer_test(_Config) ->
    L = [
         { [a, [{a, <<"2">>}], 0], 2 },
         { [a, [{a, "2"}],     0], 2 },
         { [a, [],             0], 0 },

         { [a, [{a, "2"}], 0, 1], 1 },
         { [a, [{a, "2"}], 0, 2], 2 },
         { [a, [{a, "2"}], 0, 3], 2 },

         { [a, [{a, "2"}], 0, 1, 1], 1 },
         { [a, [{a, "2"}], 0, 2, 1], 2 },
         { [a, [{a, "2"}], 0, 3, 3], 3 }
        ],
    [ E = test(get_value_as_integer, A) || {A, E} <- L ].

get_value_as_list_test(_Config) ->
    L = [
         { [a, [{a, true}],    ""], "true" },
         { [a, [{a, <<"1">>}], ""], "1" },
         { [a, [{a, 1.0}],     ""], "1.00000000000000000000e+00" },
         { [a, [{a, 1}],       ""], "1" },
         { [a, [],             ""], "" }
        ],
    [ E = test(get_value_as_list, A) || {A, E} <- L ].

%% == internal ==

test(Function, Args) ->
    baseline_ct:test(baseline_lists, Function, Args).
