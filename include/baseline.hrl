-ifndef(baseline).
-define(baseline, true).

%% == define ==

-define(IS_ENDIANNESS(T), (little =:= T orelse native =:= T orelse big =:= T)).
-define(IS_NEG_INTEGER(T), (is_integer(T) andalso (0 > T))).
-define(IS_NON_NEG_INTEGER(T), (is_integer(T) andalso (0 =< T))).
-define(IS_ID(T), (not(is_pid(T)))).
-define(IS_POS_INTEGER(T), (is_integer(T) andalso (0 < T))).
-define(IS_TIMEOUT(T), (infinity =:= T orelse (is_integer(T) andalso (0 =< T)))).

-define(IS_SET(A, B), (A band B =/= 0)).

%% == type ==

-type(endianness() :: little|native|big).
-type(id() :: term()).

-endif. % baseline
