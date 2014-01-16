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

-module(baseline_drv_port).

-include("internal.hrl").

%% -- public --
-export([load/1, unload/1]).
-export([open/1, open/2, close/1]).
-export([call/3, command/3, control/3]).
-export([find/1]).

%% == public ==

-spec load([property()]) -> {ok,baseline_drv()}|{error,_}.
load(Configs)
  when is_list(Configs) ->
    try lists:foldl(fun setup/2, setup(), Configs) of
        #baseline_drv{name=undefined} ->
            {error, badarg};
        #baseline_drv{path=P,name=N}=H ->
            case erl_ddll:load(P, N) of
                ok ->
                    {ok, H};
                {error, Reason} ->
                    ok = error_logger:error_report([erl_ddll:format_error(Reason),H]),
                    {error, Reason}
            end
    catch
        Reason ->
            {error, Reason}
    end.

-spec unload(baseline_drv()) -> ok|{error,_}.
unload(#baseline_drv{name=N})
  when is_list(N) ->
    erl_ddll:unload(N).


-spec open(baseline_drv()) -> {ok,port()}|{error,_}.
open(#baseline_drv{settings=L}=H) ->
    open(H, L).

-spec open(baseline_drv(),[property()]) -> {ok,port()}|{error,_}.
open(#baseline_drv{name=N}, Settings)
  when is_list(N), is_list(Settings) ->
    try open_port({spawn_driver,N}, [binary|proplists:delete(binary,Settings)]) of
        Port when is_port(Port) ->
            {ok, Port}
    catch
        error:Reason ->
            {error, Reason}
    end;
open(_Handle, _Settings) ->
    {error, badarg}.

-spec close(port()|atom()) -> ok.
close(Port)
  when is_port(Port) or is_atom(Port) ->
    true = port_close(Port),
    ok.


-spec call(port()|atom(),integer(),[term()]) -> term()|{error,_}.
call(Port, Command, Args)
  when (is_port(Port) or is_atom(Port)), is_integer(Command), is_list(Args) ->
    try erlang:port_call(Port, Command, Args) of
        Term when not is_binary(Term) ->
            Term
    catch
        error:badarg ->
            {error, badarg}
    end.

-spec command(port()|atom(),integer(),[term()]) -> ok|{error,_}.
command(Port, Command, Args)
  when (is_port(Port) or  is_atom(Port)), is_integer(Command), is_list(Args) ->
    try port_command(Port, term_to_binary({Command,Args})) of
        true ->
            ok
    catch
        error:badarg ->
            {error, badarg}
    end.

-spec control(port()|atom(),integer(),[term()]) -> term()|{error,_}.
control(Port, Command, Args)
  when (is_port(Port) or is_atom(Port)), is_integer(Command), is_list(Args) ->
    try port_control(Port, Command, term_to_binary(Args)) of
        Term when is_binary(Term) ->
            binary_to_term(Term)
    catch
        error:badarg ->
            {error, badarg}
    end.


-spec find(string()|baseline_drv()) -> {ok,port()}|{error,_}.
find(Name)
  when is_list(Name) ->
    F = fun(E) -> Name =:= proplists:get_value(name,erlang:port_info(E)) end,
    case lists:filter(F, erlang:ports()) of
        [Port] ->
            Port;
        [] ->
            {error, badarg}
    end;
find(#baseline_drv{name=N}) ->
    find(N).

%% == private ==

setup() ->
    #baseline_drv{path = baseline_app:lib_dir(undefined), settings = []}.

setup({path,Term}, #baseline_drv{}=H) ->
    if is_binary(Term) -> H#baseline_drv{path = binary_to_list(Term)};
       is_list(Term)   -> H#baseline_drv{path = Term};
       true -> throw({badarg,path})
    end;
setup({name,Term}, #baseline_drv{}=H) ->
    if is_binary(Term) -> H#baseline_drv{name = binary_to_list(Term)};
       is_list(Term)   -> H#baseline_drv{name = Term};
       true -> throw({badarg,name})
    end;
setup({settings,Term}, #baseline_drv{}=H) ->
    if is_list(Term) -> H#baseline_drv{settings = Term};
       true -> throw({badarg,settings})
    end;
setup(_Ignore, #baseline_drv{}=H) ->
    H.
