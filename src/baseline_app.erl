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
-export([basename/1, basename/2]).
-export([deps/1, env/1, version/1]).

%% == public ==

-spec basename(term()) -> atom().
basename(Term)
  when is_list(Term) ->
    basename(Term, "_app");
basename(Term)
  when is_atom(Term) ->
    basename(atom_to_list(Term)).

-spec basename(term(),string()) -> atom().
basename(Term, Suffix)
  when is_list(Term), is_list(Suffix) ->
    list_to_atom(filename:basename(Term, Suffix));
basename(Term, Suffix)
  when is_atom(Term), is_list(Suffix) ->
    basename(atom_to_list(Term), Suffix).

-spec deps(atom()) -> [atom()].
deps(App)
  when is_atom(App) ->
    _ = application:load(App),
    {ok, List} = application:get_key(App, applications),
    lists:foldl(fun proplists:delete/2, List, [kernel,stdlib]).

-spec env(atom()) -> [property()].
env(App)
  when is_atom(App) ->
    _ = application:load(App),
    List = application:get_all_env(App),
    lists:foldl(fun proplists:delete/2, List, [included_applications]).

-spec version(atom()) -> [non_neg_integer()].
version(App) ->
    _ = application:load(App),
    {ok, List} = application:get_key(App, vsn),
    lists:map(fun list_to_integer/1, string:tokens(List, ".")).
