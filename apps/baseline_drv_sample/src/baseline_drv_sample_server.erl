-module(baseline_drv_sample_server).

-include("../../../src/internal.hrl").

%% -- public --
-export([start_link/1]).
-export([call/3, call/4, cast/3]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --
-record(state, {
          id :: id(),
          port :: undefined|port()
         }).

%% == public ==

-spec start_link(id()) -> {ok, pid()}|{error, _}.
start_link(Id)
  when ?IS_ID(Id) ->
    L = [
         {spawn_driver, baseline_drv_sample},
         [
          stderr_to_stdout,
          binary,
          {parallelism, true} % TODO
         ]
        ],
    case gen_server:start_link(?MODULE, [Id], []) of
        {ok, Pid} ->
            case gen_server:call(Pid, {setup, L}, infinity) of
                ok ->
                    {ok, Pid};
                {error, Reason} ->
                    ok = gen_server:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


-spec call(pid(), integer(), term()) -> {ok, term()}|{error, _}.
call(Pid, Operation, Data)
  when is_pid(Pid), is_integer(Operation) ->
    call(Pid, Operation, Data, timer:seconds(5)).

-spec call(pid(), integer(), term(), timeout()) -> {ok, term()}|{error, _}.
call(Pid, Operation, Data, Timeout)
  when is_pid(Pid), is_integer(Operation), ?IS_TIMEOUT(Timeout) ->
    Ref = cast(Pid, Operation, Data),
    receive
        {Ref, Reply} ->
            true = demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, _, _, Reason} ->
            {error, Reason}
    after
        Timeout ->
            true = demonitor(Ref, [flush]),
            {error, timeout}
    end.

-spec cast(pid(), integer(), term()) -> reference().
cast(Pid, Operation, Data) ->
    Ref = monitor(process, Pid), % cnode => Exception
    ok = gen_server:cast(Pid, {command, term_to_binary([{self(), Ref}, Operation, Data])}),
    Ref.

%% -- behaviour: gen_server --

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({setup, Args}, _From, #state{id=I}=S)
  when I =/= undefined ->
    setup(Args, S).

handle_cast({command, Data}, #state{port=P}=S) ->
    try port_command(P, Data) of
        true ->
            {noreply, S}
    catch
        error:Reason ->
            {stop, {error, Reason}, S}
    end.

handle_info({P, {data, B}}, #state{port=P}=S) ->
    _ = apply(gen_server, reply, binary_to_term(B)),
    {noreply, S};
handle_info({'EXIT', P, Reason}, #state{port=P}=S) ->
    {stop, {port_close, Reason}, S#state{port = undefined}};
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State}.

%% == intarnal ==

cleanup(#state{port=P}=S)
  when P =/= undefined ->
    true = port_close(P),
    cleanup(S#state{port = undefined});
cleanup(#state{}) ->
    ok.

setup([Id]) ->
    false = process_flag(trap_exit, true),
    {ok, #state{id = Id}}.

setup(Args, #state{port=undefined}=S) ->
    try apply(erlang, open_port, Args) of
        Port ->
            {reply, ok, S#state{port = Port}}
    catch
        error:Reason ->
            {reply, {error, Reason}, S}
    end.
