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

-module(baseline_sample1_app).

%% -- behaviour: application --
-behaviour(application).
-export([start/2, stop/1]).

%% -- private --
-record(state, {
          sup :: pid()
         }).

%% == behaviour: application ==

start(_StartType, StartArgs) ->
    try lists:foldl(fun setup/2, setup(), env(StartArgs)) of
        #state{sup=P}=S when is_pid(P) ->
            {ok, P, S};
        #state{}=S ->
            ok = cleanup(S),
            {error, badarg}
    catch
        {Reason, State} ->
            ok = cleanup(State),
            {error, Reason}
    end.

stop(State) ->
    cleanup(State).

%% == private: state ==

cleanup(#state{sup=P}=S)
  when undefined =/= P ->
    _ = baseline_sup:stop(P),
    cleanup(S#state{sup = undefined});
cleanup(#state{}) ->
    baseline:flush().

setup() ->
    #state{}.

setup({sup,Term}, #state{sup=undefined}=S) ->
    Args = if is_list(Term) -> Term;
              true -> throw({badarg,sup})
           end,
    case apply(baseline_sup, start_link, Args) of
        {ok, Pid} ->
            S#state{sup = Pid};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({sup_child,Term}, #state{sup=P}=S)
  when is_pid(P) ->
    List = if is_list(Term) -> Term;
              true -> throw({badarg,sup_child})
           end,
    try lists:foldl(fun setup_child/2, P, List) of
        P ->
            S
    catch
        Reason ->
            {error, Reason}
    end;
setup(_Ignore, #state{}=S) ->
    S.

setup_child(Term, SupRef) ->
    Spec = if is_tuple(Term), 6 =:= size(Term) -> Term;
              is_list(Term) -> Term;
              true -> throw({badarg,sup_child})
           end,
    case supervisor:start_child(SupRef, Spec) of
        {ok, _Pid} ->
            SupRef;
        {error, Reason} ->
            throw(Reason)
    end.

%% == private ==

env(List) ->
    env(baseline_sample1, List).

env(App, List) ->
    baseline_lists:merge(baseline_app:env(App), List).
