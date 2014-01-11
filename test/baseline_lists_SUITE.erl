%% =============================================================================
%% =============================================================================

-module(baseline_lists_SUITE).

-compile(export_all).

-include("internal.hrl").

all() -> [
          {group, group_public}
         ].

groups() ->
    [
     {group_public, [], [
                         choose_test,
                         equalize_test,
                         except_test,
                         merge_test
                        ]}
    ].

%% == group: public ==

choose_test(_Config) ->
    D = [
         { [[a,b,c]] }
        ],
    undefined = execute(choose, [[]]),
    [ true = lists:member(execute(choose,A), lists:nth(1,A)) || {A} <- D ].

equalize_test(_Config) ->
    D = [
         { [20,1], [20] },
         { [20,2], [10,20] },
         { [20,3], [7,14,20] },
         { [20,4], [5,10,15,20] },
         { [20,5], [4,8,12,16,20] },
         { [20,6], [4,8,11,14,17,20] },
         { [20,0], {error,badarg} },
         { [20,-1],{error,badarg} },
         { [0,8],  {error,badarg} },
         { [-1,8], {error,badarg} }
        ],
    [ E = execute(equalize,A) || {A,E} <- D ].

except_test(_Config) ->
    D = [
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
    [ E = execute(except,A) || {A,E} <- D ].

merge_test(_Config) ->
    D = [
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
    [ E = execute(merge,A) || {A,E} <- D ].

%% == ==

execute(Function,Args) ->
    Value = apply(baseline_lists, Function, Args),
    ct:log("func=~p, args=~p, value=~p", [Function, Args, Value]),
    Value.
