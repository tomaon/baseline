-module(baseline_app).

-include("internal.hrl").

%% -- public --
-export([children/1]).
-export([endianness/0]).
-export([find/2]).
-export([version/1]).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

%% -- internal --
-record(state, {
          drivers :: undefined|[iolist()|atom()],
          pid     :: undefined|pid()
         }).

%% == public ==

-spec children(sup_ref()) -> [pid()].
children(SupRef)
  when ?IS_SUP_REF(SupRef) ->
    [ C || {_, C, _, _} <- supervisor:which_children(SupRef), C =/= undefined ].


-spec endianness() -> endianness().
endianness() ->
    Env = get_env(),
    case is_list(Env) andalso lists:keyfind(endianness, 1, Env) of
        false ->
            case <<1:16/native>> of
                <<1, 0>> -> little;
                <<0, 1>> -> big
            end;
        Endianness ->
            Endianness
    end.


-spec find(sup_ref(), id()) -> pid()|undefined.
find(SupRef, Id)
  when ?IS_SUP_REF(SupRef), ?IS_ID(Id) ->
    try lists:keyfind(Id, 1, supervisor:which_children(SupRef)) of
        false ->
            undefined;
        Tuple ->
            element(2, Tuple)
    catch
        error:noproc ->
            undefined
    end.


-spec version(application()) -> [non_neg_integer()].
version(Application)
  when ?IS_APPLICATION(Application) ->
    lists:map(fun list_to_integer/1, string:tokens(get_key(Application, vsn, []), ".")).

%% -- behaviour: application --

start(StartType, []) ->
    start(StartType, get_env());
start(_StartType, StartArgs) ->
    try lists:foldl(fun setup/2, setup(), StartArgs) of
        #state{pid=P}=S ->
            {ok, P, S}
    catch
        {Reason, State} ->
            ok = cleanup(State),
            {error, Reason}
    end.

stop(State) ->
    cleanup(State).

%% -- behaviour: supervisor --

init(Args) ->
    {ok, Args}.

%% == internal ==

cleanup(#state{drivers=D}=S)
  when D =/= undefined ->
    ok = lists:foreach(fun erl_ddll:unload_driver/1, D),
    cleanup(S#state{drivers = undefined});
cleanup(#state{}) ->
    ok.

setup() ->
    #state{}.

setup({driver, Drivers}, State) ->
    lists:foldl(fun setup_driver/2, State#state{drivers = []}, Drivers);
setup({sup, [SupName, {_, ChildSpecs}=A]}, State) ->
    check_childspecs(ChildSpecs, State) andalso
        setup_sup(SupName, A, State);
setup({sup_child, ChildSpecs}, State) ->
    check_childspecs(ChildSpecs, State) andalso
        lists:foldl(fun setup_sup_child/2, State, ChildSpecs);
setup(_, State) ->
    State.

setup_driver({Path, Name}, #state{drivers=D}=S) ->
    case erl_ddll:load_driver(Path, Name) of
        ok ->
            S#state{drivers = [Name|D]};
        {error, Reason} ->
            throw({Reason, S})
    end;
setup_driver(Name, State) ->
    case code:lib_dir(Name, priv) of
        {error, Reason} ->
            throw({Reason, State});
        Path ->
            setup_driver({Path, Name}, State)
    end.

setup_sup(SupName, Args, State) ->
    case supervisor:start_link(SupName, ?MODULE, Args) of
        {ok, Pid} ->
            State#state{pid = Pid};
        ignore ->
            throw({badarg, State});
        {error, Reason} ->
            throw({Reason, State})
    end.

setup_sup_child(ChildSpec, #state{pid=P}=S) ->
    case supervisor:start_child(P, ChildSpec) of
        {ok, _Child} ->
            S;
        {ok, _Child, _Info} ->
            S;
        {error, Reason} ->
            throw({Reason, S})
    end.


check_childspecs(ChildSpecs, State) ->
    case supervisor:check_childspecs(ChildSpecs) of
        ok ->
            true;
        {error, Reason} ->
            throw({Reason, State})
    end.

get_env() ->
    case application:get_application() of
        {ok, Application} ->
            application:get_all_env(Application);
        undefined ->
            undefined
    end.

get_key(Application, Key, Default) ->
    case application:get_key(Application, Key) of
        {ok, Val} ->
            Val;
        undefined ->
            Default
    end.
