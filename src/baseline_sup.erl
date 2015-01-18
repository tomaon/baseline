%% =============================================================================
%% Copyright 2014-2015 AONO Tomohiko
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

-module(baseline_sup).

-include("internal.hrl").

%% -- public --
-export([start_link/1, start_link/2, stop/1]).
-export([cast/2]).
-export([find/2]).
-export([children/1]).

%% -- behaviour: supervisor --
-behaviour(supervisor).
-export([init/1]).

%% == public ==

-spec start_link(term()) -> startlink_ret().
start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

-spec start_link(sup_name(), term()) -> startlink_ret().
start_link(SupName, Args) ->
    supervisor:start_link(SupName, ?MODULE, Args).

-spec stop(sup_ref()) -> stop_ret().
stop(SupRef) ->
    stop(SupRef, children(SupRef)).


-spec cast(sup_ref(),term()) -> [pid()].
cast(SupRef, Term) ->
    cast(SupRef, Term, [],
         [ {C,M} || {_,C,_,[M|_]} <- supervisor:which_children(SupRef), is_pid(C) ]).


-spec find(sup_ref(),term()) -> pid()|undefined.
find(SupRef, Id) ->
    case [ C || {I,C,_,_} <- supervisor:which_children(SupRef), Id =:= I ] of
        [] ->
            undefined;
        [Child] ->
            Child
    end.


-spec children(sup_ref()) -> [pid()].
children(SupRef) ->
    [ C || {_,C,_,_} <- supervisor:which_children(SupRef), is_pid(C) ].

%% == behaviour: supervisor ==

init(Args) ->
    {ok, Args}.

%% == private ==

cast(_SupRef, _Term, List, []) ->
    List;
cast(SupRef, Term, List, [{C,M}|T]) ->
    ok = apply(M, cast, [C,Term]),
    cast(SupRef, Term, [C|List], T).

stop(SupRef, []) when is_pid(SupRef) ->
    true = exit(SupRef, normal),
    ok;
stop(SupRef, []) when is_atom(SupRef) -> % TODO, {atom(),node()}, {global,atom()}
    stop(whereis(SupRef), []);
stop(SupRef, [H|T]) ->
    _ = supervisor:terminate_child(SupRef, H),
    stop(SupRef, T).
