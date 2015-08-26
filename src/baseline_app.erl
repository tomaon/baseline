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

-module(baseline_app).

-include("internal.hrl").

%% -- public --
-export([ensure_start/1, ensure_start/2]).
-export([loaded/1, loaded_applications/0]).
-export([running/1, running_applications/0]).
-export([deps/1, args/2, registered/1, version/1]).
-export([get_key/2, get_key/3]).
-export([lib_dir/1, lib_dir/2]).

%% -- behaviour: application --
-behaviour(application).
-export([start/2, prep_stop/1, stop/1]).
-export([start_phase/5]).

%% -- private --
-record(state, {
          sup :: pid()
         }).

%% == public ==

-spec ensure_start(atom()) -> ok|{error,_}.
ensure_start(Application)
  when is_atom(Application) ->
    ensure_start(Application, temporary).

-spec ensure_start(atom()|[atom()],atom()) -> ok|{error,_}.
ensure_start([], Type)
  when is_atom(Type) ->
    ok;
ensure_start([H|T]=L, Type)
  when is_atom(H), is_atom(Type) ->
    case application:start(H, Type) of
        ok ->
            ensure_start(T, Type);
        {error, {already_started,H}} ->
            ensure_start(T, Type);
        {error, {not_started,Application}} ->
            ensure_start([Application|L], Type);
        {error, Reason} ->
            {error, Reason}
    end;
ensure_start(Application, Type)
  when is_atom(Application), is_atom(Type) ->
    ensure_start([Application], Type).


-spec loaded(atom()) -> boolean().
loaded(Application)
  when is_atom(Application) ->
    lists:member(Application, loaded_applications()).

-spec loaded_applications() -> [atom()].
loaded_applications() ->
    [ element(1,E) || E <- application:loaded_applications() ].


-spec running(atom()) -> boolean().
running(Application)
  when is_atom(Application) ->
    lists:member(Application, running_applications()).

-spec running_applications() -> [atom()].
running_applications() ->
    [ element(1,E) || E <- application:which_applications() ].


-spec deps(atom()) -> [atom()].
deps(Application)
  when is_atom(Application) ->
    F = fun (E) ->
                get_key(E, applications)
        end,
    ensure_call(F, Application).

args(List) ->
    {ok, Application} = application:get_application(),
    args(Application, List).

-spec args(atom(),proplists:proplist()) -> proplists:proplist().
args(Application, List) ->
    case env(Application) of
        {error, Reason} ->
            {error, Reason};
        Env ->
            baseline_lists:merge(Env, List)
    end.

-spec registered(atom()) -> [atom()].
registered(Application)
  when is_atom(Application) ->
    F = fun (E) ->
                get_key(E, registered)
        end,
    ensure_call(F, Application).

-spec version(atom()) -> [term()].
version(Application)
  when is_atom(Application) ->
    F = fun (E) ->
                lists:map(fun(L) -> try list_to_integer(L) catch _:_ -> L end end,
                          string:tokens(get_key(E,vsn,[]), "."))
        end,
    ensure_call(F, Application).


-spec get_key(atom(),atom()) -> term().
get_key(Application, Key)
  when is_atom(Application), is_atom(Key) ->
    get_key(Application, Key, undefined).

-spec get_key(atom(),atom(),term()) -> term().
get_key(Application, Key, DefaultVal)
  when is_atom(Application), is_atom(Key) ->
    case application:get_key(Application, Key) of
        {ok, Val} ->
            Val;
        undefined ->
            DefaultVal
    end.


-spec lib_dir(atom()) -> file:filename().
lib_dir(Application)
  when is_atom(Application) ->
    filename:join(lib_dir(Application,priv), "lib").

-spec lib_dir(atom(),atom()) -> file:filename().
lib_dir(Application, SubDir)
  when is_atom(Application), is_atom(SubDir)->
    case code:lib_dir(Application, SubDir) of
        {error, bad_name} ->
            {ok, Dir} = file:get_cwd(),
            filename:join(Dir, atom_to_list(SubDir));
        Dir ->
            Dir
    end.

%% == behaviour: application ==

start(_StartType, StartArgs) ->
    try lists:foldl(fun setup/2, setup(), args(StartArgs)) of
        #state{sup=P}=S when is_pid(P) ->
            {ok, P, S};
        #state{}=S ->
            ok = cleanup(S),
            {error, noproc}
    catch
        {Reason, #state{}=S} ->
            ok = cleanup(S),
            {error, Reason}
    end.

prep_stop(State) ->
    cleanup(State).

stop(ok) ->
    void.


start_phase(Application, Phase, StartType, PhaseArgs, Fun)
  when is_atom(Application), is_atom(Phase), is_function(Fun) ->
    case Fun(Phase, StartType, PhaseArgs) of
        ok ->
            ok;
        {error, Reason} ->
            [ baseline_sup:stop(E) || E <- ?MODULE:registered(Application) ],
            {error, Reason}
    end.

%% == private: state ==

cleanup(#state{sup=P}=S)
  when undefined =/= P ->
    _ = baseline_sup:stop(P),
    cleanup(S#state{sup = undefined});
cleanup(#state{}) ->
    ok.

setup() ->
    #state{}.

setup({sup,Term}, #state{sup=undefined}=S) ->
    Args = if is_list(Term), ((1 =:= length(Term)) or (2 =:= length(Term))) -> Term;
              true -> throw({{badarg,sup},S})
           end,
    case apply(baseline_sup, start_link, Args) of
        {ok, Pid} ->
            S#state{sup = Pid};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({sup_child,Term}, #state{sup=P}=S)
  when is_pid(P) ->
    List = if is_list(Term) -> Term;
              true -> throw({{badarg,sup_child},S})
           end,
    try lists:foldl(fun setup_child/2, P, List) of
        P ->
            S
    catch
        Reason ->
            throw({Reason,S})
    end;
setup(_Ignore, #state{}=S) ->
    S.

setup_child(Term, SupRef) ->
    Spec = if is_tuple(Term), 6 =:= size(Term) -> Term;
              is_list(Term) -> Term;
              true -> throw({badarg,sup_child})
           end,
    case supervisor:start_child(SupRef, Spec) of
        {ok, _Pid} ->
            SupRef;
        {error, Reason} ->
            throw(Reason)
    end.

%% == private ==

ensure_call(Fun, Application) ->
    ensure_call(Fun, Application, loaded(Application)).

ensure_call(Fun, Application, true) ->
    Fun(Application);
ensure_call(Fun, Application, false) ->
    case application:load(Application) of
        ok ->
            ensure_call(Fun, Application, true);
        {error, Reason} ->
            {error, Reason}
    end.

env(Application)
  when is_atom(Application) ->
    env(Application, [included_applications]).

env(Application, Exclude)
  when is_atom(Application), is_list(Exclude) ->
    F = fun (E) ->
                List = application:get_all_env(E),
                lists:foldl(fun proplists:delete/2, List, Exclude)
        end,
    ensure_call(F, Application).
