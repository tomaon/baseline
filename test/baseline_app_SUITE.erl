%% =============================================================================
%% =============================================================================

-module(baseline_app_SUITE).

-compile(export_all).

-include("internal.hrl").

all() -> [
          {group, group_public},
          {group, group_app}
         ].

groups() ->
    [
     {group_public, [], [
                         start_test,
                         stop_test,
                         loaded_test,
                         loaded_applications_test,
                         deps_test,
                         env_test,
                         lib_dir_test,
                         version_test
                        ]},
     {group_app, [], [app_test]}
    ].

init_per_suite(Config) ->
    L = [
         otp_release
        ],
    Config ++ [ {E,erlang:system_info(E)} || E <- L ].

%% == group: app ==

app_test(_Config) ->
    X = [
         { [baseline],  ok },
         { [undefined], {error,baseline_ct:enoent(undefined)} }
        ],
    F = fun (A) ->
                case execute(start, A) of
                    ok ->
                        execute(stop, A);
                    {error, Reason} ->
                        {error, Reason}
                end
        end,
    [ E = F(A) || {A,E} <- X ].

%% == group: public ==

start_test(_Config) ->
    {skip, not_implemented}. % >> app_test

stop_test(_Config) ->
    {skip, not_implemented}. % >> app_test

loaded_test(_Config) ->
    X = [
         { [kernel],    true },
         { [stdlib],    true },
         { [crypto],    false },
         { [baseline],  false },
         { [undefined], false }
        ],
    [ E = execute(loaded,A) || {A,E} <- X ].

loaded_applications_test(Config) ->
    loaded_applications_test(Config, ?config(otp_release,Config)).

loaded_applications_test(_Config, Release) when "R16B" < Release ->
    [kernel,common_test,stdlib] = execute(loaded_applications, []);
loaded_applications_test(_Config, _Release) ->
    [kernel,stdlib] = execute(loaded_applications, []).

deps_test(_Config) ->
    X = [
         { [kernel],    [] },
         { [stdlib],    [kernel] },
         { [crypto],    [kernel,stdlib] },
         { [baseline],  [kernel,stdlib,crypto] },
         { [undefined], {error,baseline_ct:enoent(undefined)} }
        ],
    [ E = execute(deps,A) || {A,E} <- X ].

env_test(_Config) ->
    X = [
         { [kernel],    [{error_logger,tty}] },
         { [stdlib],    [] },
         { [crypto],    [] },
         { [baseline],  [{environment,src}] },
         { [undefined], {error,baseline_ct:enoent(undefined)} }
        ],
    [ E = execute(env,A) || {A,E} <- X ].

lib_dir_test(_Config) ->
    X = [
         { [kernel],    filename:join([code:lib_dir(kernel,priv),lib]) },
         { [stdlib],    filename:join([code:lib_dir(stdlib,priv),lib]) },
         { [crypto],    filename:join([code:lib_dir(crypto,priv),lib]) },
         { [baseline],  filename:join([baseline_ct:base_dir(),priv,lib]) },
         { [undefined], filename:join([baseline_ct:base_dir(0),priv,lib]) }
        ],
    [ E = execute(lib_dir,A) || {A,E} <- X ].

version_test(_Config) ->
    case file:read_file(filename:join([baseline_ct:base_dir(),"VERSION"])) of
        {ok, Binary} ->
            E = string:strip(binary_to_list(Binary), right, $\n), % TODO
            V = execute(version,[baseline]),
            A = string:join(lists:map(fun integer_to_list/1, V),"."),
            E = A;
        {error, Reason} ->
            ct:fail(Reason)
    end.

%% == ==

execute(Function, Args) ->
    baseline_ct:execute(baseline_app, Function, Args).
