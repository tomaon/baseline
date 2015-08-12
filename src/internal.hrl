%% =============================================================================
%% =============================================================================

-include("../include/baseline.hrl").

%% == type ==

%% -- file --
-type filename() :: file:filename().

%% -- proplists --
-type property() :: proplists:property().

%% -- supervisor --
-type child() :: undefined| pid().
-type child_id() :: term().
-type startlink_err() :: term().
-type startlink_ret() :: supervisor:startchild_ret().
-type sup_name() :: {local,atom()}|{global,atom()}|{via,module(),any()}.
-type sup_ref() :: atom()|{atom(),node()}|{global,atom()}|{via,module(),any()}|pid().
