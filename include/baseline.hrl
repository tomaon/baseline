%% =============================================================================
%% =============================================================================

%% == define ==

-define(IS_BOOLEAN(T), (true =:= T orelse false =:= T)).

-define(IS_ENDIANNESS(T), (big =:= T orelse little =:= T)).

-define(IS_NEG_INTEGER(T), (is_integer(T) andalso (0 > T))).

-define(IS_NON_NEG_INTEGER(T), (is_integer(T) andalso (0 =< T))).

-define(IS_POS_INTEGER(T), (is_integer(T) andalso (0 < T))).

-define(IS_TIMEOUT(T), (infinity =:= T orelse (is_integer(T) andalso (0 =< T)))).

%% == type ==

-type(endianness() :: big|little).

-export_type([endianness/0]).
