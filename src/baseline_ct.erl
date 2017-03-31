-module(baseline_ct).

-include("internal.hrl").

%% -- public --
-export([setup/1]).
-export([test/3]).
-export([loop/3]).

%% == public ==

-spec setup(atom()) -> ok.
setup(Key) ->
    set_env(ct:get_config(Key, [])).


-spec test(atom(), atom(), [term()]) -> term().
test(Module, Function, Args)
  when is_atom(Module), is_atom(Function), is_list(Args) ->
    Value = try apply(Module, Function, Args)
            catch
                C:E ->
                    {'catch', C, E}
            end,
    ct:log("{m,f,a}={~p,~p,~p} -> ~p", [Module, Function, Args, Value]),
    Value.


-spec loop(atom(), [term()], [term()]) -> term().
loop(Type, Config, List)
  when is_atom(Type), is_list(Config), is_list(List) ->
    try lists:foldl(fun(E, A) -> E(A) end, Config, List)
    catch
        Reason ->
            case Type of
                fail -> ct:fail(Reason);
                skip -> {skip, Reason}
            end
    end.

%% == internal ==

set_env([]) ->
    ok;
set_env([{A, L}|T]) ->
    [ ok = test(application, set_env, [A, P, V, [{persistent, true}]]) || {P, V} <- L ],
    set_env(T).
