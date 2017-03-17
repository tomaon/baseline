-module(baseline).

-include("internal.hrl").

%% -- public --
-export([start/1, start/2, stop/1]).
-export([flush/0]).

-type(reason() :: {atom(), _}).

-export_type([reason/0]).

%% == public ==

-spec start(application()) -> ok|{error, reason()}.
start(Application) ->
    start(Application, temporary).

-spec start(application(), restart_type()) -> ok|{error, reason()}.
start(Application, RestartType)
  when ?IS_APPLICATION(application), ?IS_RESTART_TYPE(RestartType) ->
    case application:ensure_all_started(Application, RestartType) of
        {ok, _Started} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(application()) -> ok|{error, reason()}.
stop(Application)
  when ?IS_APPLICATION(application) ->
    application:stop(Application).


-spec flush() -> ok.
flush() ->
    receive
        _ -> flush()
    after
        0 -> ok
    end.
