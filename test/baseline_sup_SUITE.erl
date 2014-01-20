%% =============================================================================
%% =============================================================================

-module(baseline_sup_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).

%% -- public --
-export([
         start_link_test/1, stop_test/1,
         find_test/1
        ]).

%% == callback: ct ==

all() -> [
          start_link_test, stop_test,
          find_test
         ].

%% == public ==

start_link_test(_Config) ->
    X = [
         %% -- one_for_one --
         {
           [% 0, named
            {local, baseline_sup},
            {
              {one_for_one, 0, 1},
              [
              ]
            }
           ],
           {ok, 0}
         },
         {
           [% 0
            {
              {one_for_one, 0, 1},
              [
              ]
            }
           ],
           {ok, 0}
         },
         {
           [% 1
            {
              {one_for_one, 0, 1},
              [
               { baseline_fsm,
                 {baseline_fsm,start_link,[[]]},
                 temporary, 5000, worker,
                 [baseline_server,gen_fsm] }
              ]
            }
           ],
           {ok, 1}
         },
         %% -- simple_one_for_one --
         %% {
         %%   [% 0, named
         %%    {local, baseline_sup},
         %%    {
         %%      {simple_one_for_one, 0, 1},
         %%      [
         %%      ]
         %%    }
         %%   ],
         %%   ? % bad_start_spec, TODO
         %% },
         %% {
         %%   [% 0
         %%    {
         %%      {simple_one_for_one, 0, 1},
         %%      [
         %%      ]
         %%    }
         %%   ],
         %%   ? % bad_start_spec, TODO
         %% },
         {
           [% 1
            {
              {simple_one_for_one, 0, 1},
              [
               { baseline_fsm,
                 {baseline_fsm,start_link,[[]]},
                 temporary, 5000, worker,
                 [baseline_server,gen_fsm] }
              ]
            }
           ],
           {ok, 0}
         }
        ],
    F = fun (E) ->
                case test(start_link, E) of
                    {ok, Pid} ->
                        V = length(supervisor:which_children(Pid)),
                        _ = test(stop, [Pid]),
                        {ok, V};
                    {error, Reason} ->
                        {error, Reason}
                end
        end,
    [ E = F(A) || {A,E} <- X ].


stop_test(_Config) ->
    X = [
         {
           baseline_sup,
           ok
         }
        ],
    L = [
         {local, baseline_sup},
         {
           {one_for_one, 0, 1},
           [
            { baseline_fsm,
              {baseline_fsm,start_link,[[]]},
              temporary, 5000, worker,
              [baseline_server,gen_fsm] }
           ]
         }
        ],
    F = fun (A) ->
                case test(start_link, L) of
                    {ok, Pid} ->
                        case test(stop, [A]) of
                            ok ->
                                ok;
                            {error, Reason} ->
                                _ = test(stop, [Pid]),
                                {error, Reason}
                        end;
                    {error, Reason} ->
                        {error, Reason}
                end
        end,
    [ E = F(A) || {A,E} <- X ].


find_test(_Config) ->
    X = [
         {
           baseline_fsm,
           false
         },
         {
           {baseline_fsm,1},
           true
         },
         {
           {baseline_fsm,2},
           false
         }
        ],
    L = [
         {
           {one_for_one, 0, 1},
           [
            { {baseline_fsm,1},
              {baseline_fsm,start_link,[[]]},
              temporary, 5000, worker,
              [baseline_server,gen_fsm] }
           ]
         }
        ],
    F = fun (E) ->
                case test(start_link, L) of
                    {ok, Pid} ->
                        V = is_pid(test(find, [Pid,E])),
                        _ = test(stop, [Pid]),
                        V;
                    {error, Reason} ->
                        {error, Reason}
                end
        end,
    [ E = F(A) || {A,E} <- X ].

%% == private ==

test(Function, Args) ->
    baseline_ct:test(baseline_sup, Function, Args).
