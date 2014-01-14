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

-module(baseline_ct).

-include("internal.hrl").

%% -- public --
-export([base_dir/0, base_dir/1]).
-export([enoent/1]).
-export([execute/3]).

%% == public ==

-spec base_dir() -> filename().
base_dir() ->
    base_dir(2).

-spec base_dir(non_neg_integer()) -> filename().
base_dir(N)
  when is_integer(N), 0 =< N ->
    case file:get_cwd() of % ~/.ct/ct_run.test@HOST.YYYY-MM-DD_hh.mm.ss
        {ok, Dir} ->
            L = filename:split(Dir),
            filename:join(lists:sublist(L,length(L) - N));
        {error, Reason} ->
            ct:fail(Reason)
    end.


-spec enoent(atom()) -> {string(),string()}.
enoent(App) ->
    {"no such file or directory", atom_to_list(App) ++ ".app"}.


-spec execute(atom(),atom(),[term()]) -> term().
execute(Module, Function, Args) ->
    Value = apply(Module, Function, Args),
    ct:log("{m,f,a}={~p,~p,~p} -> ~p", [Module,Function,Args,Value]),
    Value.
