%% =============================================================================
%% Copyright 2014 AONO Tomohiko
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License version 2.1 as published by the Free Software Foundation.
%%
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%% =============================================================================

-module(baseline_app).

-include("internal.hrl").

%% -- public --
-export([start/1, start/2, stop/1]).
-export([loaded/1, loaded_applications/0]).
-export([deps/1, env/1, lib_dir/1, lib_dir/2, version/1]).

%% == public ==

-spec start(atom()) -> ok|{error,_}.
start(Application)
  when is_atom(Application) ->
    start(Application, temporary).

-spec start(atom(),atom()) -> ok|{error,_}.
start(Application, Type)
  when is_atom(Application), is_atom(Type) ->
    ensure_start([Application], Type).

-spec stop(atom()) -> ok|{error,_}.
stop(Application)
  when is_atom(Application) ->
    application:stop(Application).


-spec loaded(atom()) -> boolean().
loaded(Application)
  when is_atom(Application) ->
    lists:member(Application, loaded_applications()).

-spec loaded_applications() -> [atom()].
loaded_applications() ->
    lists:map(fun(E) -> element(1,E) end, application:loaded_applications()).


-spec deps(atom()) -> [atom()].
deps(Application)
  when is_atom(Application) ->
    F = fun (E) ->
                {ok, List} = application:get_key(E, applications),
                List
        end,
    ensure_call(F, Application).

-spec env(atom()) -> [term()].
env(Application)
  when is_atom(Application) ->
    F = fun (E) ->
                List = application:get_all_env(E),
                lists:foldl(fun proplists:delete/2, List, [included_applications])
        end,
    ensure_call(F, Application).

-spec lib_dir(atom()) -> filename().
lib_dir(Application)
  when is_atom(Application) ->
    filename:join(lib_dir(Application,priv), "lib").

-spec lib_dir(atom(),atom()) -> filename().
lib_dir(Application, SubDir)
  when is_atom(Application), is_atom(SubDir)->
    case code:lib_dir(Application, SubDir) of
        {error, bad_name} ->
            {ok, Dir} = file:get_cwd(),
            filename:join(Dir, atom_to_list(SubDir));
        Dir ->
            Dir
    end.

-spec version(atom()) -> [term()].
version(Application)
  when is_atom(Application) ->
    F = fun (E) ->
                {ok, List} = application:get_key(E, vsn),
                lists:map(fun(T) -> try list_to_integer(T) catch _:_ -> T end end,
                          string:tokens(List, "."))
        end,
    ensure_call(F, Application).

%% == private ==

ensure_call(Fun, Application) ->
    ensure_call(Fun, Application, loaded(Application)).

ensure_call(Fun, Application, true) ->
    Fun(Application);
ensure_call(Fun, Application, false) ->
    case application:load(Application) of
        ok ->
            ensure_call(Fun, Application, true);
        {error, Reason} ->
            {error, Reason}
    end.

ensure_start([], _Type) ->
    ok;
ensure_start([H|T]=L, Type) ->
    case application:start(H, Type) of
        ok ->
            ensure_start(T, Type);
        {error, {already_started,H}} ->
            ensure_start(T, Type);
        {error, {not_started,Application}} ->
            ensure_start([Application|L], Type);
        {error, Reason} ->
            {error, Reason}
    end.
