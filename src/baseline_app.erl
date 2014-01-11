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
-export([start/1, stop/1]).
-export([loaded/1, loaded_applications/0]).
-export([deps/1, env/1, lib_dir/1, lib_dir/2, version/1]).

%% == public ==

-spec start(atom()) -> ok.
start(Application)
  when is_atom(Application) ->
    L = baseline_lists:merge(loaded_applications(), deps(Application)),
    lists:foreach(fun application:start/1, L ++ [Application]).

-spec stop(atom()) -> ok.
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
    execute(F, Application).

-spec env(atom()) -> [property()].
env(Application)
  when is_atom(Application) ->
    F = fun (E) ->
                List = application:get_all_env(E),
                lists:foldl(fun proplists:delete/2, List, [included_applications])
        end,
    execute(F, Application).

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

-spec version(atom()) -> [non_neg_integer()].
version(Application)
  when is_atom(Application) ->
    F = fun (E) ->
                {ok, List} = application:get_key(E, vsn),
                lists:map(fun list_to_integer/1, string:tokens(List, "."))
        end,
    execute(F, Application).

%% == private ==

execute(Fun, Application) ->
    execute(Fun, Application, loaded(Application)).

execute(Fun, Application, true) ->
    Fun(Application);
execute(Fun, Application, false) ->
    case application:load(Application) of
        ok ->
            execute(Fun, Application, true);
        {error, Reason} ->
            {error, Reason}
    end.
