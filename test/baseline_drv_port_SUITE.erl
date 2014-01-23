%% =============================================================================
%% =============================================================================

-module(baseline_drv_port_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).

%% -- pubic --
-export([
         call_test/1, command_test/1, control_test/1
        ]).

%% == callback: ct ==

all() -> [
          %% @see haseline_drv_SUITE
          call_test, command_test, control_test
         ].

%% == public ==

call_test(_Config) ->
    X = [
         { [undefined,0,[]], {error,badarg} }
        ],
    [ E = test(call,A) || {A,E} <- X ].

command_test(_Config) ->
    X = [
         { [undefined,0,[]], {error,badarg} }
        ],
    [ E = test(command,A) || {A,E} <- X ].

control_test(_Config) ->
    X = [
         { [undefined,0,[]], {error,badarg} }
        ],
    [ E = test(control,A) || {A,E} <- X ].

%% == private ==

test(Function, Args) ->
    baseline_ct:test(baseline_drv_port, Function, Args).
