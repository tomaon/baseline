%% =============================================================================
%% =============================================================================

-module(baseline_app_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

%% -- public --
-export([
         loaded_test/1, loaded_applications_test/1,
         deps_test/1, env_test/1, lib_dir_test/1, version_test/1,
         start_stop_test/1
        ]).

%% == callback: ct ==

all() -> [
          loaded_test, loaded_applications_test,
          deps_test, env_test, lib_dir_test, version_test,
          start_stop_test
         ].

init_per_suite(Config) ->
    L = [
         otp_release
        ],
    Config ++ [ {E,erlang:system_info(E)} || E <- L ].

end_per_suite(Config) ->
    Config.

%% == public ==

loaded_test(_Config) ->
    X = [
         { [kernel],    true },
         { [stdlib],    true },
         { [crypto],    false },
         { [baseline],  false },
         { [undefined], false }
        ],
    [ E = test(loaded,A) || {A,E} <- X ].

loaded_applications_test(Config) ->
    loaded_applications_test(Config, ?config(otp_release,Config)).

loaded_applications_test(_Config, Release) when "R16B" < Release ->
    [kernel,common_test,stdlib] = test(loaded_applications, []);
loaded_applications_test(_Config, _Release) ->
    [kernel,stdlib] = test(loaded_applications, []).


deps_test(_Config) ->
    X = [
         { [kernel],    [] },
         { [stdlib],    [kernel] },
         { [crypto],    [kernel,stdlib] },
         { [baseline],  [kernel,stdlib,crypto] },
         { [undefined], {error,baseline_ct:enoent(undefined)} }
        ],
    [ E = test(deps,A) || {A,E} <- X ].

env_test(_Config) ->
    X = [
         { [kernel],    [{error_logger,tty}] },
         { [stdlib],    [] },
         { [crypto],    [] },
         { [baseline],  [{environment,src}] },
         { [undefined], {error,baseline_ct:enoent(undefined)} }
        ],
    [ E = test(env,A) || {A,E} <- X ].

lib_dir_test(_Config) ->
    X = [
         { [kernel],    filename:join([code:lib_dir(kernel,priv),lib]) },
         { [stdlib],    filename:join([code:lib_dir(stdlib,priv),lib]) },
         { [crypto],    filename:join([code:lib_dir(crypto,priv),lib]) },
         { [baseline],  filename:join([baseline_ct:base_dir(),priv,lib]) },
         { [undefined], filename:join([baseline_ct:base_dir(0),priv,lib]) }
        ],
    [ E = test(lib_dir,A) || {A,E} <- X ].

version_test(_Config) ->
    case file:read_file(filename:join([baseline_ct:base_dir(),"VERSION"])) of
        {ok, Binary} ->
            E = string:strip(binary_to_list(Binary), right, $\n), % TODO
            V = test(version,[baseline]),
            A = string:join(lists:map(fun integer_to_list/1, V),"."),
            E = A;
        {error, Reason} ->
            ct:fail(Reason)
    end.


start_stop_test(_Config) -> % MUST be the last
    X = [
         %% kernel
         { [stdlib],    ok },
         { [crypto],    ok },
         { [baseline],  ok },
         { [undefined], {error,baseline_ct:enoent(undefined)} }
        ],
    F = fun (A) ->
                case test(start, A) of
                    ok ->
                        test(stop, A);
                    {error, Reason} ->
                        {error, Reason}
                end
        end,
    [ E = F(A) || {A,E} <- X ].

%% == private ==

test(Function, Args) ->
    baseline_ct:test(baseline_app, Function, Args).
