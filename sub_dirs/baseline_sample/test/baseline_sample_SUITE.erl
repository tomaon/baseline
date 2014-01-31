%% =============================================================================
%% =============================================================================

-module(baseline_sample_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0]).

%% -- public --
-export([
         start_test/1
        ]).

%% == callback: ct ==

all() -> [
          start_test
         ].

%% == public ==

start_test(_Config) ->
    X = [
         %% -- one_for_one --
         {
           [% 0-0, named
            {sup, [
                   {local, baseline_sample_sup},
                   {
                     {one_for_one, 0, 1},
                     [
                     ]
                   }
                  ]},
            {sup_child, [
                        ]}
           ],
           ok
         },
         {
           [% 0-0
            {sup, [
                   {
                     {one_for_one, 0, 1},
                     [
                     ]
                   }
                  ]},
            {sup_child, [
                        ]}
           ],
           ok
         },
         {
           [% 0-1
            {sup, [
                   {
                     {one_for_one, 0, 1},
                     [
                     ]
                   }
                  ]},
            {sup_child, [
                         { baseline_server,
                           {baseline_server,start_link,[[]]},
                           temporary, 5000, worker,
                           [baseline_server,gen_server] }
                        ]}
           ],
           ok
         },
         {
           [% 1-0
            {sup, [
                   {
                     {one_for_one, 0, 1},
                     [
                      { baseline_fsm,
                        {baseline_fsm,start_link,[[]]},
                        temporary, 5000, worker,
                        [baseline_server,gen_fsm] }
                     ]
                   }
                  ]},
            {sup_child, [
                        ]}
           ],
           ok
         },
         {
           [% 1-1
            {sup, [
                   {
                     {one_for_one, 0, 1},
                     [
                      { baseline_fsm,
                        {baseline_fsm,start_link,[[]]},
                        temporary, 5000, worker,
                        [baseline_server,gen_fsm] }
                     ]
                   }
                  ]},
            {sup_child, [
                         { baseline_server,
                           {baseline_server,start_link,[[]]},
                           temporary, 5000, worker,
                           [baseline_server,gen_server] }
                        ]}
           ],
           ok
         },
         %% -- simple_one_for_one --
         {
           [% 0-0, named
            {sup, [
                   {local, baseline_sample_sup},
                   {
                     {simple_one_for_one, 0, 1},
                     [
                     ]
                   }
                  ]},
            {sup_child, [
                        ]}
           ],
           {error,{bad_start_spec,[]}}
         },
         {
           [% 0-0
            {sup, [
                   {
                     {simple_one_for_one, 0, 1},
                     [
                     ]
                   }
                  ]},
            {sup_child, [
                        ]}
           ],
           {error,{bad_start_spec,[]}}
         },
         {
           [% 0-1
            {sup, [
                   {
                     {simple_one_for_one, 0, 1},
                     [
                     ]
                   }
                  ]},
            {sup_child, [
                         { baseline_server,
                           {baseline_server,start_link,[[]]},
                           temporary, 5000, worker,
                           [baseline_server,gen_server] }
                        ]}
           ],
           {error,{bad_start_spec,[]}}
         },
         {
           [% 1-0
            {sup, [
                   {
                     {simple_one_for_one, 0, 1},
                     [
                      { baseline_fsm,
                        {baseline_fsm,start_link,[[]]},
                        temporary, 5000, worker,
                        [baseline_server,gen_fsm] }
                     ]
                   }
                  ]},
            {sup_child, [
                        ]}
           ],
           ok
         },
         %% {
         %%   [ % 1-1
         %%    {sup, [
         %%           {
         %%             {simple_one_for_one, 0, 1},
         %%             [
         %%              { baseline_fsm,
         %%                {baseline_fsm,start_link,[[]]},
         %%                temporary, 5000, worker,
         %%                [baseline_server,gen_fsm] }
         %%             ]
         %%           }
         %%          ]},
         %%    {sup_child, [
         %%                 { baseline_server,
         %%                   {baseline_server,start_link,[[]]},
         %%                   temporary, 5000, worker,
         %%                   [baseline_server,gen_server] }
         %%                ]}
         %%   ],
         %%   {error,{'EXIT',{badarg,_}}
         %% },
         {
           [% 1-1
            {sup, [
                   {
                     {simple_one_for_one, 0, 1},
                     [
                      { baseline_server,
                        {baseline_server,start_link,[[]]},
                        temporary, 5000, worker,
                        [baseline_server,gen_server] }
                     ]
                   }
                  ]},
            {sup_child, [
                         [1]
                        ]}
           ],
           ok
         },
         %% -- error --
         {
           [
           ],
           {error,badarg}
         },
         {
           [
            {sup, [
                  ]},
            {sup_child, [
                        ]}
           ],
           {error,{badarg,sup}}
         },
         {
           [
            {sup, [
                   {
                     {one_for_one, 0, 1},
                     [
                     ]
                   }
                  ]},
            {sup_child, {} }
           ],
           {error,{badarg,sup_child}}
         },
         {
           [
            {sup, [
                   {
                     {one_for_one, 0, 1},
                     [
                     ]
                   }
                  ]},
            {sup_child, [
                         {}
                        ]}
           ],
           {error,{badarg,sup_child}}
         },
         {
           [
            {sup, [
                   {
                     {one_for_one, 0, 1},
                     [
                     ]
                   }
                  ]},
            {sup_child, [
                         { baseline_server,
                           {baseline_server,start_link,[]},
                           temporary, 5000, undefined,
                           [baseline_server,gen_server] }
                        ]}
           ],
           {error,{invalid_child_type,undefined}}
         }
        ],
    [ E = test(A) || {A,E} <- X ].

%% == private ==

test(Args) ->
    test(baseline_sample, Args).

test(Name, Args) ->
    [ apply(application,F, [Name]) || F <- [ unload, load ] ],
    [ application:set_env(Name,K,V) || {K,V} <- Args ],
    V = case apply(Name, start, []) of
            ok ->
                _ = apply(Name, stop, []),
                ok;
            {error, {Reason,_}} ->
                {error, Reason}
        end,
    ct:log("env=~p -> ~p", [baseline_app:env(Name),V]),
    V.
