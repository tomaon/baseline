%% =============================================================================
%% =============================================================================

-module(baseline_drv_port_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).

%% -- pubic --
-export([
         load_test/1,
         call_test/1, command_test/1, control_test/1,
         find_test/1
        ]).

%% == callback: ct ==

all() -> [
          %% @see haseline_drv_SUITE
          load_test,
          call_test,
          command_test,
          control_test,
          find_test
         ].

%% == public ==

load_test(_Config) ->
    X = [
         { [[]],                     {error,badarg} },
         { [[{path,undefined}]],     {error,{badarg,path}} },
         { [[{name,undefined}]],     {error,{badarg,name}} },
         { [[{name,"undefined"}]],   {error,{open_error,-10}} },
         { [[undefined]],            {error,badarg} }
        ],
    [ E = test(load,A) || {A,E} <- X ].


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


find_test(_Config) ->
    X = [
         { ["undefined"], {error,badarg} }
        ],
    [ E = test(find,A) || {A,E} <- X ].

%% == private ==

test(Function, Args) ->
    baseline_ct:test(baseline_drv_port, Function, Args).
