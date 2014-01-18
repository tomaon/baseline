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
-export([start_link/1, start_link/2, stop/1]).

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
    stop(SupRef, [ element(2,E) || E <- supervisor:which_children(SupRef) ]).

%% == behaviour: supervisor ==

init(Args) ->
    {ok, Args}.

%% == private ==

stop(SupRef, []) when is_pid(SupRef) ->
    true = exit(SupRef, normal),
    ok;
stop(SupRef, []) when is_atom(SupRef) -> % TODO, {atom(),node()}, {global,atom()}
    stop(whereis(SupRef), []);
stop(SupRef, [H|T]) ->
    _ = supervisor:terminate_child(SupRef, H),
    stop(SupRef, T).
