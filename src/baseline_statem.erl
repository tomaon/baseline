-module(baseline_statem).

-include("internal.hrl").

%% -- public --
-export([start_link/1, start_link/2]).

-behaviour(gen_statem).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([loaded/3, initialized/3, ready/3]). % state_functions

%% -- internal --
-record(data, {
          id :: integer()
         }).

%% == public ==

-spec start_link([term()]) -> {ok, pid()}|{error, _}.
start_link(Args)
  when is_list(Args) ->
    start_link(Args, 0).

-spec start_link([term()], id()) -> {ok, pid()}|{error, _}.
start_link(Args, Id)
  when is_list(Args), ?IS_ID(Id) ->
    case gen_statem:start_link(?MODULE, [Id], []) of
        {ok, Pid} ->
            try gen_statem:call(Pid, {setup, Args}, infinity) of
                ok ->
                    {ok, Pid};
                {error, Reason} ->
                    ok = gen_statem:stop(Pid),
                    {error, Reason}
            catch
                exit:Reason ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% -- behaviour: gen_statem --

init(Args) ->
    setup(Args).

callback_mode() ->
    state_functions.

terminate(_Reason, _State, Data) ->
    cleanup(Data).

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {ok, OldState, OldData}.


loaded(info, initialize, Data) ->
    {next_state, initialized, Data}.

initialized({call, From}, {setup, Args}, #data{id=I}=D)
  when I =/= undefined ->
    try lists:foldl(fun setup/2, D, Args) of
        Data ->
            {next_state, ready, Data, {reply, From, ok}}
    catch
        {Reason, Data} ->
            {keep_state, Data, {reply, From, {error, Reason}}}
    end.

ready(enter, _OldState, Data) ->
    {keep_state, Data};
ready(info, {'EXIT', _Pid, Reason}, Data) ->
    {stop, Reason, Data}.

%% == internal ==

cleanup(#data{}) ->
    baseline:flush().

setup([Id]) ->
    false = process_flag(trap_exit, true),
    {ok, loaded, #data{id = Id}, {next_event, info, initialize}}.

setup(_Ignore, Data) ->
    Data.
