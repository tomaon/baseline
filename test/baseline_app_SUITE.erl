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

%% == group: app ==

app_test(_Config) ->
    L = [
         { [baseline],  ok },
         { [undefined], {error,{undefined,enoent(undefined)}} }
        ],
    F = fun (A) ->
                case execute(start, A) of
                    {ok, _List} ->
                        execute(stop, A);
                    {error, Reason} ->
                        {error, Reason}
                end
        end,
    [ E = F(A) || {A,E} <- L ].

%% == group: public ==

start_test(_Config) ->
    {skip, not_implemented}. % >> app_test

stop_test(_Config) ->
    {skip, not_implemented}. % >> app_test

loaded_test(_Config) ->
    D = [
         { [kernel],    true },
         { [stdlib],    true },
         { [crypto],    false },
         { [baseline],  false },
         { [undefined], false }
        ],
    [ E = execute(loaded,A) || {A,E} <- D ].

loaded_applications_test(_Config) ->
    [kernel,common_test,stdlib] = execute(loaded_applications, []).

deps_test(_Config) ->
    D = [
         { [kernel],    [] },
         { [stdlib],    [kernel] },
         { [crypto],    [kernel,stdlib] },
         { [baseline],  [kernel,stdlib,crypto] },
         { [undefined], {error,enoent(undefined)} }
        ],
    [ E = execute(deps,A) || {A,E} <- D ].

env_test(_Config) ->
    D = [
         { [kernel],    [{error_logger,tty}] },
         { [stdlib],    [] },
         { [crypto],    [] },
         { [baseline],  [{environment,src}] },
         { [undefined], {error,enoent(undefined)} }
        ],
    [ E = execute(env,A) || {A,E} <- D ].

lib_dir_test(_Config) ->
    D = [
         { [kernel],    filename:join([code:lib_dir(kernel,priv),lib]) },
         { [stdlib],    filename:join([code:lib_dir(stdlib,priv),lib]) },
         { [crypto],    filename:join([code:lib_dir(crypto,priv),lib]) },
         { [baseline],  filename:join([base_dir(),priv,lib]) },
         { [undefined], filename:join([base_dir(0),priv,lib]) }
        ],
    [ E = execute(lib_dir,A) || {A,E} <- D ].

version_test(_Config) ->
    case file:read_file(filename:join([base_dir(),"VERSION"])) of
        {ok, Binary} ->
            E = string:strip(binary_to_list(Binary), right, $\n), % TODO
            V = execute(version,[baseline]),
            A = string:join(lists:map(fun integer_to_list/1, V),"."),
            E = A;
        {error, Reason} ->
            ct:fail(Reason)
    end.

%% == ==

base_dir() ->
    base_dir(2).

base_dir(N) ->
    case file:get_cwd() of % ~/.ct/ct_run.test@HOST.YYYY-MM-DD_hh.mm.ss
        {ok, Dir} ->
            L = filename:split(Dir),
            filename:join(lists:sublist(L, length(L) - N));
        {error, Reason} ->
            ct:fail(Reason)
    end.

enoent(App) ->
    {"no such file or directory",atom_to_list(App) ++ ".app"}.

execute(Function,Args) ->
    Value = apply(baseline_app, Function, Args),
    ct:log("func=~p, args=~p, value=~p", [Function, Args, Value]),
    Value.
