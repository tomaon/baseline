%% =============================================================================
%% =============================================================================

-include("../include/baseline.hrl").

%% == type ==

-type filename() :: file:filename().
-type property() :: proplists:property().
-type startchild_ret() :: {ok,pid()}|{ok,pid(),term()}|{error,_}.
-type startlink_ret() :: {ok,pid()}|ignore|{error,_}.
-type stop_ret() :: ok.
-type sup_name() :: {local,atom()}|{global,atom()}.
-type sup_ref() :: atom()|{atom(),node()}|{global,atom()}|pid().
-type terminatechild_ret() :: ok|{error,_}.
