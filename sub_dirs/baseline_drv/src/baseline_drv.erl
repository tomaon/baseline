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

-module(baseline_drv).

-include("internal.hrl").

%% -- public --
-export([start_link/2, stop/1]).
-export([call/4, call/5]).
-export([load/1, unload/1]).
-export([find/1]).

%% -- callback: poolboy_worker --
-export([start_link/1]).

%% -- behaviour: gen_server --
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- private --
-record(state, {
          port :: port(),
          assigned :: {pid(),_}
         }).

%% == public ==

-spec start_link(string()|binary(),[property()]) -> {ok,pid()}|{error,_}.
start_link(Name, Settings)
  when is_list(Name), is_list(Settings) ->
    case gen_server:start_link(?MODULE, [], []) of
        {ok, Pid} ->
            case gen_server:call(Pid, {setup,Name,Settings}, infinity) of
                ok ->
                    {ok, Pid};
                {error, Reason} ->
                    ok = stop(Pid),
                    {error, Reason}
            end
    end;
start_link(Name, Settings)
  when is_binary(Name), is_list(Settings) ->
    start_link(binary_to_list(Name), Settings).

-spec stop(pid()) -> ok.
stop(Pid)
  when is_pid(Pid) ->
    gen_server:call(Pid, stop, infinity).


-spec call(pid(),atom(),integer(),[term()]) -> term()|{error,_}.
call(Pid, Function, Command, Args)
  when is_pid(Pid), is_atom(Function), is_integer(Command), is_list(Args) ->
    call(Pid, Function, Command, Args, timer:seconds(5)).

-spec call(pid(),atom(),integer(),[term()],timeout()) -> term()|{error,_}.
call(Pid, Function, Command, Args, Timeout)
  when is_pid(Pid), is_atom(Function), is_integer(Command), is_list(Args) ->
    gen_server:call(Pid, {Function,Command,Args}, Timeout).


-spec load([property()]) -> ok|{error,_}.
load(Configs)
  when is_list(Configs) ->
    baseline_drv_port:load(Configs).

-spec unload(string()|binary()) -> ok|{error,_}.
unload(Name)
  when is_list(Name) ->
    baseline_drv_port:unload(Name);
unload(Name)
  when is_binary(Name) ->
    unload(binary_to_list(Name)).


-spec find(string()|binary()) -> {ok,port()}|{error,_}.
find(Name)
  when is_list(Name) ->
    baseline_drv_port:find(Name);
find(Name)
  when is_binary(Name) ->
    find(binary_to_list(Name)).

%% == callback: poolboy_worker ==

-spec start_link([term()]) -> {ok,pid()}|{error,_}.
start_link(Args)
  when is_list(Args), 2 =:= length(Args) ->
    apply(?MODULE, start_link, Args).

%% == behaviour: gen_server ==

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({command,Command,Args}, From, #state{port=P,assigned=undefined}=S)
  when is_integer(Command), is_list(Args)->
    case baseline_drv_port:command(P, Command, Args) of
        ok ->
            {noreply, S#state{assigned = From}};
        {error, Reason} ->
            {reply, {error,Reason}, S}
    end;
handle_call({Function,Command,Args}, _From, #state{port=P,assigned=undefined}=S)
  when is_atom(Function), is_integer(Command), is_list(Args)->
    {reply, apply(baseline_drv_port,Function,[P,Command,Args]), S};
handle_call(_Request, _From, #state{assigned=A}=S)
  when undefined =/= A ->
    {reply, {error,ebusy}, S};
handle_call({setup,Name,Settings}, _From, #state{}=S) ->
    case baseline_drv_port:open(Name, Settings) of
        {ok, Port} ->
            {reply, ok, S#state{port = Port}};
        {error, Reason} ->
            {reply, {error,Reason}, S}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error,badarg}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({P,{data,Data}}, #state{port=P,assigned=undefined}=S)
  when is_binary(Data) ->
    {stop, {error,ebadmsg}, S};
handle_info({P,{data,Data}}, #state{port=P,assigned=A}=S)
  when is_binary(Data) ->
    case binary_to_term(Data) of
        ack ->
            {noreply, S}; % async
        Term ->
            _ = gen_server:reply(A, Term),
            {noreply, S#state{assigned = undefined}}
    end;
handle_info({'EXIT',_Pid,Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% == private: state ==

cleanup(#state{port=P}=S)
  when undefined =/= P ->
    _ = baseline_drv_port:close(P),
    cleanup(S#state{port = undefined});
cleanup(#state{}) ->
    baseline:flush().

setup([]) ->
    _ = process_flag(trap_exit, true),
    {ok, #state{}}.
