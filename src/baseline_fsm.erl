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

-module(baseline_fsm).

-include("internal.hrl").

%% -- public --
-export([start_link/1, start_link/2, stop/1]).
-export([call/2, cast/2]).

%% -- behaviour: gen_fms --
-behaviour(gen_fsm).
-export([init/1, terminate/3, code_change/4,
         handle_event/3, handle_sync_event/4, handle_info/3]).

-export([loaded/3, ready/2, ready/3]).

%% -- internal --
-record(state, {
          id :: integer()
         }).

%% == public ==

-spec start_link(proplists:proplist()) -> {ok,pid()}|{error,_}.
start_link(Args)
  when is_list(Args) ->
    start_link(Args, 0).

-spec start_link(proplists:proplist(),integer()) -> {ok,pid()}|{error,_}.
start_link(Args, Id)
  when is_list(Args), is_integer(Id) ->
    case gen_fsm:start_link(?MODULE, [Id], []) of
        {ok, Pid} ->
            case gen_fsm:sync_send_event(Pid, {setup,Args}, infinity) of
                ok ->
                    {ok, Pid};
                {error, Reason} ->
                    ok = stop(Pid),
                    {error, Reason}
            end
    end.

-spec stop(pid()) -> ok.
stop(Pid)
  when is_pid(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, stop, infinity).


-spec call(pid(),term()) -> term().
call(Pid, Term)
  when is_pid(Pid) ->
    gen_fsm:sync_send_event(Pid, Term).

-spec cast(pid(),term()) -> ok.
cast(Pid, Term)
  when is_pid(Pid) ->
    gen_fsm:send_event(Pid, Term).

%% == behaviour: gen_fsm ==

init(Args) ->
    io:format("~p [~p:init] args=~w~n",[self(),?MODULE,Args]),
    setup(Args).

terminate(_Reason, _StateName, StateData) ->
    io:format("~p [~p:terminate] ~p=~p~n", [self(),?MODULE,_StateName,_Reason]),
    cleanup(StateData).

code_change(_OldVsn, StateName, StateData, _Extra) ->
    io:format("~p [~p:code_change] extra=~p(~p)~n", [self(),?MODULE,_Extra,_OldVsn]),
    {ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    io:format("~p [~p:handle_event] ~p=~p~n", [self(),?MODULE,StateName,_Event]),
    {next_state, StateName, StateData}.

handle_sync_event(stop, _From, _StateName, StateData) ->
    {stop, normal, ok, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    io:format("~p [~p:handle_sync_event] ~p=~p~n", [self(),?MODULE,StateName,_Event]),
    {reply, {error,badarg}, StateName, StateData}.

handle_info({'EXIT',_Pid,Reason}, _StateName, StateData) ->
    {stop, Reason, StateData};
handle_info(_Info, StateName, StateData) ->
    io:format("~p [~p:handle_info] ~p=~p~n", [self(),?MODULE,StateName,_Info]),
    {next_state, StateName, StateData}.

loaded({setup,Args}, _From, State) ->
    try lists:foldl(fun setup/2, State, Args) of
        #state{}=S ->
            {reply, ok, ready, S}
    catch
        {Reason, #state{}=S} ->
            {reply, {error,Reason}, loaded, S}
    end;
loaded(_Event, _From, StateData) ->
    io:format("~p [~p:loaded] event=~p~n",[self(),?MODULE,_Event]),
    {reply, {error,badarg}, loaded, StateData}.

ready(_Event, StateData) ->
    io:format("~p [~p:ready] event=~p~n", [self(),?MODULE,_Event]),
    {next_state, ready, StateData}.

ready(_Event, _From, StateData) ->
    io:format("~p [~p:ready] event=~p~n",[self(),?MODULE,_Event]),
    {reply, ok, ready, StateData}.

%% == internal ==

cleanup(#state{}) ->
    baseline:flush().

setup([Id]) ->
    _ = process_flag(trap_exit, true),
    {ok, loaded, #state{id = Id}}.

setup(_Ignore, #state{}=S) ->
    S.
