%% =============================================================================
%% =============================================================================

-module(baseline_sample_drv_app).

%% -- behaviour: application --
-behaviour(application).
-export([start/2, prep_stop/1, stop/1]).

%% -- private --
-record(wrapper, {
          driver :: [atom()],
          state :: tuple()
         }).

%% == behaviour: application ==

start(StartType, StartArgs) ->
    L = args(StartArgs),
    try lists:foldl(fun setup/2, setup(), proplists:get_value(resource,L,[])) of
        #wrapper{}=W ->
            case baseline_app:start(StartType, proplists:get_value(process,L,[])) of
                {ok, Pid, State} ->
                    {ok, Pid, W#wrapper{state = State}};
                {error, Reason} ->
                    ok = cleanup(W),
                    {error, Reason}
            end
    catch
        {Reason, #wrapper{}=W} ->
            ok = cleanup(W),
            {error, Reason}
    end.

prep_stop(#wrapper{state=S}=W) ->
    _ = baseline_app:prep_stop(S),
    ok = cleanup(W),
    S.

stop(State) ->
    baseline_app:stop(State).

%% == private: state ==

cleanup(#wrapper{driver=D}=W)
  when undefined =/= D ->
    [ baseline_drv:unload(E) || E <- D ],
    cleanup(W#wrapper{driver = undefined});
cleanup(#wrapper{}) ->
    ok.

setup() ->
    #wrapper{driver = []}.

setup({driver,Term}, #wrapper{driver=D}=W) ->
    Args = if is_list(Term) -> Term;
              true -> throw({{badarg,driver},W})
           end,
    case baseline_drv:load(Args) of
        ok ->
            W#wrapper{driver = [proplists:get_value(name,Args)|D]};
        {error, Reason} ->
            throw({Reason,W})
    end;
setup(_Ignore, #wrapper{}=W) ->
    W.

%% == private ==

args(List) ->
    args(baseline_sample_drv, List).

args(App, List) ->
    baseline_lists:merge(baseline_app:env(App), List).
