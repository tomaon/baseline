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

-module(baseline_sup).

-include("internal.hrl").

%% -- public --
-export([start_link/2, stop/1]).
-export([start_child/2, terminate_child/2]).
-export([find/2]).

%% -- behaviour: supervisor --
-behaviour(supervisor).
-export([init/1]).

%% == public ==

-spec start_link(sup_name(), term()) -> startlink_ret().
start_link(SupName, Args) ->
    supervisor:start_link(SupName, ?MODULE, Args).

-spec stop(sup_ref()) -> stop_ret().
stop(SupRef) ->
    stop(SupRef, [ element(2,E) || E <- supervisor:which_children(SupRef) ]).


-spec start_child(sup_ref(),term()) -> startchild_ret().
start_child(SupRef, Args) ->
    supervisor:start_child(SupRef, Args).

-spec terminate_child(sup_ref(),pid()) -> terminatechild_ret().
terminate_child(SupRef, Pid) ->
    supervisor:terminate_child(SupRef, Pid).


-spec find(sup_ref(),term()) -> node()|undefined. % pid() -> node(), for poolboy
find(SupRef, Id) ->
    case lists:keyfind(Id, 1, supervisor:which_children(SupRef)) of
        {Id, Child, _Type, _Modules} ->
            Child;
        _ ->
            undefined
    end.

%% == behaviour: supervisor ==

init(Args) ->
    {ok, Args}.

%% == private ==

stop(SupRef, [H|T]) ->
    _ = supervisor:terminate_child(SupRef, H),
    stop(SupRef, T);
stop(SupRef, []) when is_pid(SupRef) ->
    true = exit(SupRef, normal),
    ok.
