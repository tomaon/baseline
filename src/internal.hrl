-ifndef(internal).
-define(internal, true).

-include("../include/baseline.hrl").

%% == define ==

-define(IS_APPLICATION(T), (is_atom(T))).
-define(IS_RESTART_TYPE(T), (permanent =:= T orelse transient =:= T orelse temporary =:= T)).
-define(IS_SUP_REF(T), (true)).

%% == type ==

-type(application() :: atom()).
-type(restart_type() :: permanent|transient|temporary).
-type(sup_ref() :: term()).

-endif. % internal
