-module(baseline_proclib).

%% rd(state, {n}).
%% F = fun(#state{n=N}=S) -> S#state{n = N+1} end.
%% {ok, P} = baseline_proclib:start_link([a]).
%% sys:statistics(P, true).
%% sys:trace(P, true).
%% sys:get_state(P).
%% sys:replace_state(P, F).
%% sys:get_state(P).
%% sys:statistics(P, get).
%% sys:statistics(P, false).
%% baseline_proclib:stop(P).

%% -- public --
-export([start_link/1, stop/1]).
-export([init/1]).

-export([system_continue/3, system_terminate/4, system_code_change/4,
         system_get_state/1, system_replace_state/2]).

%% -- internal --

-record(state, {
          n :: integer()
         }).

%% == public ==

-spec start_link([term()]) -> {ok, pid()}|{error, _}.
start_link(Args)
  when is_list(Args) ->
    proc_lib:start_link(?MODULE, init, [Args]).

-spec stop(pid()) -> ok.
stop(Pid)
  when is_pid(Pid) ->
    proc_lib:stop(Pid).


-spec init([term()]) -> ok. % no_return().
init(Args) ->
    case length(Args) of
        N when N > 0 ->
            ok = proc_lib:init_ack({ok, self()}),
            loop(#state{n = N}, sys:debug_options([]));
        _ ->
            ok = proc_lib:init_ack({error, badarg})
    end.

%% -- callback: sys --

system_continue(_Parent, Debug, Misc) ->
    loop(Misc, Debug).

system_terminate(_Reason, _Parent, _Debug, _Misc) -> % << loop/1
    none.

system_code_change(Misc, ?MODULE, _OldVsn, _Extra) ->
    {ok, Misc}.

system_get_state(Misc) -> % << loop/1, > system_continue/3
    {ok, Misc}.

system_replace_state(StateFun, Misc) -> % << loop/1, > system_continue/3
    M = StateFun(Misc),
    {ok, M, M}.

%% == internal ==

loop(Misc, Debug) ->
    receive
        {system, {P, _}=F, Request} ->
            D = sys:handle_debug(Debug, fun debug/3, P, Request),
            sys:handle_system_msg(Request, F, P, ?MODULE, D, Misc);
        {'EXIT', _Pid, Reason} ->
            exit(Reason)
    end.

debug(Device, Event, Extra) ->
    io:format(Device, "~p [~p:~p] ~p,~p~n", [self(), ?MODULE, ?FUNCTION_NAME, Event, Extra]).
