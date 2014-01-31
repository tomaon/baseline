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

-module(baseline_drv_app).

%% -- behaviour: application --
-behaviour(application).
-export([start/2, prep_stop/1, stop/1]).

%% -- private --
-record(wrapper, {
          driver :: [string()|binary()],
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
    args(baseline_drv, List).

args(App, List) ->
    baseline_lists:merge(baseline_app:env(App), List).
