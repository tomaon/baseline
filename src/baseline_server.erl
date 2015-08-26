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

-module(baseline_server).

-include("internal.hrl").

%% -- public --
-export([start_link/1, start_link/2, stop/1]).
-export([call/2, cast/2]).

%% -- behaviour: gen_server --
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

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
    case gen_server:start_link(?MODULE, [Id], []) of
        {ok, Pid} ->
            case gen_server:call(Pid, {setup,Args}, infinity) of
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
    gen_server:call(Pid, stop, infinity).


-spec call(pid(),term()) -> term().
call(Pid, Term)
  when is_pid(Pid) ->
    gen_server:call(Pid, Term).

-spec cast(pid(),term()) -> ok.
cast(Pid, Term)
  when is_pid(Pid) ->
    gen_server:cast(Pid, Term).

%% == behaviour: gen_server ==

init(Args) ->
    io:format("~p [~p:init] args=~w~n",[self(),?MODULE,Args]),
    setup(Args).

terminate(_Reason, State) ->
    io:format("~p [~p:terminate] reason=~p~n", [self(),?MODULE,_Reason]),
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    io:format("~p [~p:code_change] extra=~p(~p)~n", [self(),?MODULE,_Extra,_OldVsn]),
    {ok, State}.

handle_call({setup,Args}, _From, State) ->
    try lists:foldl(fun setup/2, State, Args) of
        #state{}=S ->
            {reply, ok, S}
    catch
        {Reason, #state{}=S} ->
            {reply, {error,Reason}, S}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    io:format("~p [~p:handle_call] req=~p~n", [self(),?MODULE,_Request]),
    {reply, {error,badarg}, State}.

handle_cast(_Request, State) ->
    io:format("~p [~p:handle_cast] req=~p~n", [self(),?MODULE,_Request]),
    {noreply, State}.

handle_info({'EXIT',_Pid,Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    io:format("~p [~p:handle_info] info=~p~n", [self(),?MODULE,_Info]),
    {noreply, State}.

%% == intarnal ==

cleanup(#state{}) ->
    baseline:flush().

setup([Id]) ->
    _ = process_flag(trap_exit, true),
    {ok, #state{id = Id}}.

setup(_Ignore, #state{}=S) ->
    S.
