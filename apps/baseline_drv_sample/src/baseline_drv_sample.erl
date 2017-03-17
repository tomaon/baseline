-module(baseline_drv_sample).

-include("../../../include/baseline.hrl").

%% -- public --
-export([start/0, stop/0]).

-export([start_link/1, stop/1]).
-export([foo/2, bar/2, baz/2]).

%% -- internal --
-type(reason() :: baseline:reason()).

%% == public ==

-spec start() -> ok|{error, reason()}.
start() ->
    baseline:start(?MODULE).

-spec stop() -> ok|{error, reason()}.
stop() ->
    baseline:stop(?MODULE).


-spec start_link(id()) -> {ok, pid()}|{error, _}.
start_link(Id)
  when ?IS_ID(Id) ->
    supervisor:start_child(baseline_drv_sample_sup, [Id]).

-spec stop(pid()) -> ok|{error, not_found|simple_one_for_one}.
stop(Pid)
  when is_pid(Pid) ->
    supervisor:terminate_child(baseline_drv_sample_sup, Pid).


-spec foo(pid(), integer()) -> {ok, integer()}|{error, _}.
foo(Pid, Long)
  when is_pid(Pid), is_integer(Long) ->
    baseline_drv_sample_server:call(Pid, 0, Long).

-spec bar(pid(), integer()) -> {ok, integer()}|{error, _}.
bar(Pid, Long)
  when is_pid(Pid), is_integer(Long) ->
    baseline_drv_sample_server:call(Pid, 1, Long).

-spec baz(pid(), integer()) -> {ok, integer()}|{error, _}.
baz(Pid, Long)
  when is_pid(Pid), is_integer(Long) ->
    baseline_drv_sample_server:call(Pid, 2, Long).
