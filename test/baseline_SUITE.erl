%% =============================================================================
%% =============================================================================

-module(baseline_SUITE).

-compile(export_all).

-include("internal.hrl").

all() -> [
          {group, group_public}
         ].

groups() ->
    [
     {group_public, [], [
                         flush_test
                        ]}
    ].

%% == group: public ==

flush_test(_Config) ->
    [ self() ! ping || _ <- lists:seq(1,10) ],
    ok = execute(flush, []).

%% == ==

execute(Function,Args) ->
    Value = apply(baseline, Function, Args),
    ct:log("func=~p, args=~p, value=~p", [Function, Args, Value]),
    Value.
