%% =============================================================================
%% =============================================================================

-module(baseline_lists_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).

%% -- pubic --
-export([
         choose_test/1,
         equalize_test/1,
         except_test/1,
         merge_test/1,
         combine_test/1,
         get_as_binary_test/1, get_as_boolean_test/1, get_as_integer_test/1, get_as_list_test/1
        ]).

%% == callback: ct ==

all() -> [
          choose_test,
          equalize_test,
          except_test,
          merge_test,
          combine_test,
          get_as_binary_test, get_as_boolean_test, get_as_integer_test, get_as_list_test
         ].

%% == public ==

choose_test(_Config) ->
    X = [
         { [[a,b,c]] }
        ],
    undefined = test(choose, [[]]),
    [ true = lists:member(test(choose,A), lists:nth(1,A)) || {A} <- X ].


equalize_test(_Config) ->
    X = [
         { [20,1], [20] },
         { [20,2], [10,20] },
         { [20,3], [7,14,20] },
         { [20,4], [5,10,15,20] },
         { [20,5], [4,8,12,16,20] },
         { [20,6], [4,8,11,14,17,20] }
        ],
    [ E = test(equalize,A) || {A,E} <- X ].


except_test(_Config) ->
    X = [
         { [[],[]],       [] },
         { [[a],[]],      [a] },
         { [[{a,1}],[]],  [{a,1}] },
         { [[],[z]],      [] },
         { [[],[{z,9}]],  [] },
         { [[a],[z]],     [a] },
         { [[a],[a]],     [] },
         { [[a,b],[z]],   [a,b] },
         { [[a,b],[a]],   [b] },
         { [[a,b],[a,z]], [b] },
         { [[a,b],[a,a]], [b] },
         { [[a,b],[a,b]], [] }
        ],
    [ E = test(except,A) || {A,E} <- X ].


merge_test(_Config) ->
    X = [
         { [[],[]],       [] },
         { [[a],[]],      [a] },
         { [[{a,1}],[]],  [{a,1}] },
         { [[],[z]],      [z] },
         { [[],[{z,9}]],  [{z,9}] },
         { [[a],[z]],     [a,z] },
         { [[a],[a]],     [a] },
         { [[a,b],[z]],   [a,b,z] },
         { [[a,b],[a]],   [a,b] },
         { [[a,b],[a,z]], [a,b,z] },
         { [[a,b],[a,a]], [a,b] },
         { [[a,b],[a,b]], [a,b] }
        ],
    [ E = test(merge,A) || {A,E} <- X ].


combine_test(_Config) ->
    X = [
         { [1,[{a,1},{b,2}],[{b,d},{c,e}]], [{a,1},{d,2}] },
         { [1,[{a,1},{b,2}],[{b,d},{c,e}],[a]], [{d,2}] },
         { [1,[{a,1},{b,2}],[{b,d},{c,e}],[b]], [{a,1},{d,2}] },
         { [1,[{a,1},{b,2}],[{b,d},{c,e}],[d]], [{a,1},{d,2}] }
        ],
    [ E = test(combine,A) || {A,E} <- X ].


get_as_binary_test(_Config) ->
    L = [ {a,1}, {b,"2"}, {c,<<"3">>} ],
    X = [
         { [a,1,L,<<"?">>], <<"1">> },
         { [b,1,L,<<"?">>], <<"2">> },
         { [c,1,L,<<"?">>], <<"3">> },
         { [d,1,L,<<"?">>], <<"?">> }
        ],
    [ E = test(get_as_binary,A) || {A,E} <- X ].

get_as_boolean_test(_Config) ->
    L = [ {a,true}, {b,<<"true">>}, {c,<<"True">>}, {d,"true"}, {e,"True"} ],
    X = [
         { [a,1,L,false], true },
         { [b,1,L,false], true },
         { [c,1,L,false], false },
         { [d,1,L,false], true },
         { [e,1,L,false], false },
         { [f,1,L,false], false }
        ],
    [ E = test(get_as_boolean,A) || {A,E} <- X ].

get_as_integer_test(_Config) ->
    L = [ {a,1}, {b,"2"}, {c,<<"3">>} ],
    X = [
         { [a,1,L,-1], 1 },
         { [b,1,L,-1], 2 },
         { [c,1,L,-1], 3 },
         { [d,1,L,-1], -1 },
         { [b,1,L,-1,1], 1 },
         { [b,1,L,-1,2], 2 },
         { [b,1,L,-1,3], 2 },
         { [b,1,L,-1,3,1], 2 },
         { [b,1,L,-1,3,2], 2 },
         { [b,1,L,-1,3,3], 3 }
        ],
    [ E = test(get_as_integer,A) || {A,E} <- X ].

get_as_list_test(_Config) ->
    L = [ {a,1}, {b,"2"}, {c,<<"3">>} ],
    X = [
         { [a,1,L,"?"], "1" },
         { [b,1,L,"?"], "2" },
         { [c,1,L,"?"], "3" },
         { [d,1,L,"?"], "?" }
        ],
    [ E = test(get_as_list,A) || {A,E} <- X ].

%% == private ==

test(Function,Args) ->
    baseline_ct:test(baseline_lists, Function, Args).
