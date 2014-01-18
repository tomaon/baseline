%% =============================================================================
%% =============================================================================

-module(baseline_sup_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).

%% -- public --
-export([
         start_link_test/1
        ]).

%% == callback: ct ==

all() -> [
          start_link_test
         ].

%% == public ==

start_link_test(_Config) ->
    X = [
         %% -- one_for_one --
         { [% 0, named
            {local, baseline_sup},
            {
              {one_for_one, 0, 1},
              [
              ]
            }
           ],
           ok
         },
         { [% 0
            {
              {one_for_one, 0, 1},
              [
              ]
            }
           ],
           ok
         },
         { [% 1
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
           ok
         },
         %% -- one_for_all --
         { [% 0, named
            {local, baseline_sup},
            {
              {one_for_all, 0, 1},
              [
              ]
            }
           ],
           ok
         },
         { [% 0
            {
              {one_for_all, 0, 1},
              [
              ]
            }
           ],
           ok
         },
         { [% 1
            {
              {one_for_all, 0, 1},
              [
               { baseline_fsm,
                 {baseline_fsm,start_link,[[]]},
                 temporary, 5000, worker,
                 [baseline_server,gen_fsm] }
              ]
            }
           ],
           ok
         },
         %% -- rest_for_one --
         { [% 0, named
            {local, baseline_sup},
            {
              {rest_for_one, 0, 1},
              [
              ]
            }
           ],
           ok
         },
         { [% 0
            {
              {rest_for_one, 0, 1},
              [
              ]
            }
           ],
           ok
         },
         { [% 1
            {
              {rest_for_one, 0, 1},
              [
               { baseline_fsm,
                 {baseline_fsm,start_link,[[]]},
                 temporary, 5000, worker,
                 [baseline_server,gen_fsm] }
              ]
            }
           ],
           ok
         },
         %% -- simple_one_for_one --
         %% { [% 0, named
         %%    {local, baseline_sup},
         %%    {
         %%      {simple_one_for_one, 0, 1},
         %%      [
         %%      ]
         %%    }
         %%   ],
         %%   ? % bad_start_spec, TODO
         %% },
         %% { [% 0
         %%    {
         %%      {simple_one_for_one, 0, 1},
         %%      [
         %%      ]
         %%    }
         %%   ],
         %%   ? % bad_start_spec, TODO
         %% },
         { [% 1
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
           ok
         }
        ],
    [ E = test(A) || {A,E} <- X ].

%% == private ==

test(Args) ->
    case baseline_ct:test(baseline_sup, start_link, Args) of
        {ok, Pid} ->
            _ = baseline_sup:stop(Pid),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
