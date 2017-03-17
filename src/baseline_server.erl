-module(baseline_server).

-include("internal.hrl").

%% -- public --
-export([start_link/1, start_link/2]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --
-record(state, {
          id :: integer()
         }).

%% == public ==

-spec start_link([term()]) -> {ok, pid()}|{error, _}.
start_link(Args) ->
    start_link(Args, 0).

-spec start_link([term()], id()) -> {ok, pid()}|{error, _}.
start_link(Args, Id)
  when is_list(Args), ?IS_ID(Id) ->
    case gen_server:start_link(?MODULE, [Id], []) of
        {ok, Pid} ->
            case gen_server:call(Pid, {setup, Args}, infinity) of
                ok ->
                    {ok, Pid};
                {error, Reason} ->
                    ok = gen_server:stop(Pid),
                    {error, Reason}
            end
    end.

%% -- behaviour: gen_server --

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({setup, Args}, _From, #state{id=I}=S)
  when I =/= undefined ->
    try lists:foldl(fun setup/2, S, Args) of
        State ->
            {reply, ok, State}
    catch
        {Error, State} ->
            {reply, Error, State}
    end;
handle_call(_Request, _From, State) ->
    {stop, enosys, {error, enosys}, State}.

handle_cast(_Request, State) ->
    {stop, enosys, State}.

handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {stop, enosys, State}.

%% == internal ==

cleanup(#state{}) ->
    baseline:flush().

setup([Id]) ->
    false = process_flag(trap_exit, true),
    {ok, #state{id = Id}}.

setup(_Ignore, State) ->
    State.
