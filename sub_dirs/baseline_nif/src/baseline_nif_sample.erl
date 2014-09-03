%% =============================================================================
%% =============================================================================

-module(baseline_nif_sample).

-include("internal.hrl").

%% -- public --
-export([on_load/0]).
-export([new/1, delete/1]).
-export([get/1, set/2]).

%% -- private --
-record(baseline_nif, {
          resource :: binary()
         }).

-type(baseline_nif() :: #baseline_nif{}).

%% == public ==

-on_load(on_load/0).

-spec on_load() -> ok|{error,_}.
on_load() ->
    Path = filename:join([lib_dir(baseline_nif), "baseline_nif"]),
    LoadInfo = [],
    erlang:load_nif(Path, LoadInfo).


-spec new(integer()) -> {ok,baseline_nif()}|{error,_}.
new(Id)
  when is_integer(Id) ->
    case new_nif(Id) of
        {ok, Resource} ->
            {ok, #baseline_nif{resource = Resource}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(baseline_nif()) -> ok|{error,_}.
delete(#baseline_nif{resource=R})
  when is_binary(R) ->
    delete_nif(R).


-spec get(baseline_nif()) -> {ok,[property()]}|{error,_}.
get(#baseline_nif{resource=R}) ->
    get_nif(R).

-spec set(baseline_nif(), [property()]) -> ok|{error,_}.
set(#baseline_nif{resource=R}, List) ->
    set_nif(R, List).

%% == private: nif ==

new_nif(_Id) ->
    erlang:nif_error(not_loaded).

delete_nif(_Resource) ->
    erlang:nif_error(not_loaded).


get_nif(_Resource) ->
    erlang:nif_error(not_loaded).

set_nif(_Resource, _List) ->
    erlang:nif_error(not_loaded).

%% == private ==

lib_dir(Application) ->
    baseline_app:lib_dir(Application).
