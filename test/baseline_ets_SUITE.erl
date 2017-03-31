-module(baseline_ets_SUITE).

-include_lib("common_test/include/ct.hrl").

%% -- public --
-export([all/0, groups/0,
         init_per_group/2, end_per_group/2]).
-export([tab_test/1]).
-export([delete_test/1,
         delete_all_objects_test/1,
         insert_test/1,
         insert_new_test/1,
         lookup_test/1,
         lookup_element_test/1,
         match_test/1,
         match_delete_test/1,
         member_test/1,
         select_test/1,
         select_count_test/1,
         select_delete_test/1,
         select_reverse_test/1,
         update_counter_test/1,
         update_element_test/1]).

%% == public ==

all() -> [
          {group, group_public}
         ].

groups() -> [
             {group_public, [sequence], [
                                         tab_test,
                                         insert_test,
                                         delete_test,
                                         delete_all_objects_test,
                                         insert_new_test,
                                         lookup_test,
                                         lookup_element_test,
                                         match_test,
                                         match_delete_test,
                                         member_test,
                                         update_element_test,
                                         select_test,
                                         select_count_test,
                                         update_counter_test,
                                         select_reverse_test,
                                         select_delete_test
                                        ]}
            ].

init_per_group(group_public, Config) ->
    L = [
         ordered_set, % set
         private      % protected
         %% {keypos, 1},
         %% {heir, none},
         %% {write_concurrency, false},
         %% {read_concurrency, false}
        ],
    case test(start_link, [?MODULE, L]) of
        {ok, Pid} ->
            true = unlink(Pid),
            save(Config, Pid);
        {error, Reason} ->
            ok = ct:fail(Reason)
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(group_public, Config) ->
    true = test(Config, erlang, exit, [normal]);
end_per_group(_GroupName, _Config) ->
    ok.


tab_test(Config) ->
    true = is_integer(test(Config, tab, [])).


delete_test(Config) ->
    L = [
         {[b], true}
        ],
    [ E = test(Config, delete, A) || {A, E} <- L ].

delete_all_objects_test(Config) ->
    L = [
         {[], true}
        ],
    [ E = test(Config, delete_all_objects, A) || {A, E} <- L ].

insert_test(Config) ->
    L = [
         {[{a, 1}],           true},
         {[[{b, 2}, {c, 3}]], true},
         {[{a, 9}],           true}
        ],
    [ E = test(Config, insert, A) || {A, E} <- L ].

insert_new_test(Config) ->
    L = [
         {[{a, 1}],           true},
         {[[{b, 2}, {c, 3}]], true},
         {[{a, 9}],           false}
        ],
    [ E = test(Config, insert_new, A) || {A, E} <- L ].

lookup_test(Config) ->
    L = [
         {[a], [{a, 1}]},
         {[x], []}
        ],
    [ E = test(Config, lookup, A) || {A, E} <- L ].

lookup_element_test(Config) ->
    L = [
         {[a, 2], 1},
         {[a, 3], {'catch', error, badarg}},
         {[x, 2], {'catch', error, badarg}}
        ],
    [ E = test(Config, lookup_element, A) || {A, E} <- L ].

match_test(Config) ->
    L = [
         {[{a, '$0'}], [[1]]},
         {[{x, '$0'}], []}
        ],
    [ E = test(Config, match, A) || {A, E} <- L ].

match_delete_test(Config) ->
    L = [
         {[{b, '_'}], true}
        ],
    [ E = test(Config, match_delete, A) || {A, E} <- L ].

member_test(Config) ->
    L = [
         {[a], true},
         {[x], false}
        ],
    [ E = test(Config, member, A) || {A, E} <- L ].

select_test(Config) ->
    L = [
         {[[{{a,   '$0'}, [], ['$0']}]], [10]},
         {[[{{x,   '$0'}, [], ['$0']}]], []},
         {[[{{'_', '$0'}, [], ['$0']}]], [10, 3]}
        ],
    [ E = test(Config, select, A) || {A, E} <- L ].

select_count_test(Config) ->
    L = [
         {[[{{a,   '_'}, [], [true]}]], 1},
         {[[{{x,   '_'}, [], [true]}]], 0},
         {[[{{'_', '_'}, [], [true]}]], 2}
        ],
    [ E = test(Config, select_count, A) || {A, E} <- L ].

select_delete_test(Config) ->
    L = [
         {[[{{c, '_'}, [], [true]}]], 1},
         {[[{{c, '_'}, [], []}]],     {'catch', error, badarg}}
        ],
    [ E = test(Config, select_delete, A) || {A, E} <- L ].

select_reverse_test(Config) ->
    L = [
         {[[{{a,   '$0'}, [], ['$0']}]], [15]},
         {[[{{x,   '$0'}, [], ['$0']}]], []},
         {[[{{'_', '$0'}, [], ['$0']}]], [4, 3, 15]}
        ],
    [ E = test(Config, select_reverse, A) || {A, E} <- L ].

update_counter_test(Config) ->
     L = [
          {[a, {2, 5}], 15},
          {[d, {2, 4}, {d, 0}], 4}
        ],
    [ E = test(Config, update_counter, A) || {A, E} <- L ].

update_element_test(Config) ->
    L = [
         {[a, {2, 10}], true},
         {[x, {2, 10}], false}
        ],
    [ E = test(Config, update_element, A) || {A, E} <- L ].

%% == internal ==

save(Config, Pid) ->
    [{pid, Pid}|Config].

test(Function, Args) ->
    baseline_ct:test(baseline_ets, Function, Args).

test(Config, Function, Args) ->
    test(Config, baseline_ets, Function, Args).

test(Config, Module, Function, Args) ->
    baseline_ct:test(Module, Function, [?config(pid, Config)|Args]).
