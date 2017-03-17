-module(baseline_nif_sample).

%% -- public --
-export([on_load/0]).
-on_load(on_load/0).

-export([new/1, delete/1]).
-export([get/1, set/2]).

%% -- internal --
-type(resource() :: binary()).

%% == public ==

-spec on_load() -> ok|{error, {bad_lib|load|load_failed|old_code|reload|upgrade, string()}}.
on_load() ->
    Path = filename:join([code:lib_dir(?MODULE, priv), ?MODULE]),
    LoadInfo = [],
    erlang:load_nif(Path, LoadInfo).


-spec new(integer()) -> {ok, resource()}|{error, _}.
new(Id)
  when is_integer(Id) ->
    new_nif(Id).

-spec delete(resource()) -> ok|{error, _}.
delete(Resource)
  when is_binary(Resource) ->
    delete_nif(Resource).


-spec get(resource()) -> {ok, [term()]}|{error, _}.
get(Resource)
  when is_binary(Resource) ->
    get_nif(Resource).

-spec set(resource(), [term()]) -> ok|{error, _}.
set(Resource, List)
  when is_binary(Resource), is_list(List) ->
    set_nif(Resource, List).

%% == internal ==

new_nif(_Id) ->
    erlang:nif_error(not_loaded).

delete_nif(_Resource) ->
    erlang:nif_error(not_loaded).

get_nif(_Resource) ->
    erlang:nif_error(not_loaded).

set_nif(_Resource, _List) ->
    erlang:nif_error(not_loaded).
