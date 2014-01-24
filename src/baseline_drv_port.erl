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
-export([open/2, close/1]).
-export([call/3, command/3, control/3]).
-export([find/1]).
-export([loaded/1, loaded_drivers/0]).

%% -- private --
-record(handle, {
          path :: string(),
          name :: string()
         }).

%% == public ==

-spec load([property()]) -> ok|{error,_}.
load(Configs)
  when is_list(Configs) ->
    try lists:foldl(fun setup/2, setup(), Configs) of
        #handle{name=undefined} ->
            {error, badarg};
        #handle{path=P,name=N} ->
            load(P, N, loaded(N))
    catch
        Reason ->
            {error, Reason}
    end.

-spec unload(string()) -> ok|{error,_}.
unload(Name)
  when is_list(Name) ->
    erl_ddll:unload(Name).


-spec open(string(),[property()]) -> {ok,port()}|{error,_}.
open(Name, Settings)
  when is_list(Name), is_list(Settings) ->
    try open_port({spawn_driver,Name}, [binary|proplists:delete(binary,Settings)]) of
        Port when is_port(Port) ->
            {ok, Port}
    catch
        error:Reason ->
            {error, Reason}
    end.

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


-spec find(string()) -> {ok,port()}|{error,_}.
find(Name)
  when is_list(Name) ->
    F = fun(E) -> Name =:= proplists:get_value(name,erlang:port_info(E)) end,
    case lists:filter(F, erlang:ports()) of
        [Port] ->
            Port;
        [] ->
            {error, badarg}
    end.


-spec loaded(string()) -> boolean().
loaded(Name)
  when is_list(Name) ->
    lists:member(Name, loaded_drivers()).

-spec loaded_drivers() -> [string()].
loaded_drivers() ->
    {ok, L} = erl_ddll:loaded_drivers(),
    L.

%% == private ==

load(Path, Name, false) ->
    case erl_ddll:load(Path, Name) of
        ok ->
            ok;
        {error, Reason} ->
            ok = error_logger:error_report([erl_ddll:format_error(Reason)]),
            {error, Reason}
    end;
load(_Path, _Name, true) ->
    ok.

setup() ->
    #handle{path = baseline_app:lib_dir(undefined)}.

setup({path,Term}, #handle{}=H) ->
    Path = if is_binary(Term) -> binary_to_list(Term);
              is_list(Term)   -> Term;
              true -> throw({badarg,path})
           end,
    H#handle{path = Path};
setup({name,Term}, #handle{}=H) ->
    Name = if is_binary(Term) -> binary_to_list(Term);
              is_list(Term)   -> Term;
              true -> throw({badarg,name})
           end,
    H#handle{name = Name};
setup(_Ignore, #handle{}=H) ->
    H.
