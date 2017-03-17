-module(baseline_lists).

-include("internal.hrl").

%% -- public --
-export([combine/2, combine/3]).
-export([equalize/2]).
-export([except/3]).
-export([merge/3]).
-export([get_element/5]).
-export([get_value/2, get_value/3,
         get_value_as_binary/3,
         get_value_as_boolean/3,
         get_value_as_float/3, get_value_as_float/4, get_value_as_float/5,
         get_value_as_integer/3, get_value_as_integer/4, get_value_as_integer/5,
         get_value_as_list/3]).

%% == public ==

-spec combine([tuple()], [tuple()]) -> [tuple()].
combine(List1, List2)
  when is_list(List1), is_list(List2) ->
    lists:map(fun ({K, V}) ->
                      case lists:keyfind(K, 1, List2) of
                          false ->
                              {K, V};
                          {K, T} ->
                              {T, V}
                      end
              end, List1).

-spec combine([tuple()], [tuple()], [term()]) -> [tuple()].
combine(List1, List2, Excludes)
  when is_list(List1), is_list(List2), is_list(Excludes) ->
    except(1, combine(List1, List2), proplists:unfold(Excludes)).


-spec equalize(pos_integer(), pos_integer()) -> [pos_integer()].
equalize(N, W)
  when ?IS_POS_INTEGER(N), ?IS_POS_INTEGER(W) ->
    equalize(min(N, W), 0, N div W, N rem W, []).

equalize(0, _Sum, _Div, _Rem, List) ->
    lists:reverse(List);
equalize(Num, Sum, Div, 0, List) ->
    E = Sum + Div,
    equalize(Num - 1, E, Div, 0, [E|List]);
equalize(Num, Sum, Div, Rem, List) ->
    E = Sum + Div + 1,
    equalize(Num - 1, E, Div, Rem - 1, [E|List]).


-spec except(pos_integer(), [tuple()], [tuple()]) -> [tuple()].
except(N, List1, List2)
  when ?IS_POS_INTEGER(N), is_list(List1), is_list(List2) ->
    lists:filter(fun(E) -> false =:= lists:keyfind(element(N, E), N, List2) end, List1).


-spec merge(pos_integer(), [tuple()], [tuple()]) -> [tuple()].
merge(N, List1, List2)
  when ?IS_POS_INTEGER(N), is_list(List1), is_list(List2) ->
    lists:keysort(N, List2 ++ baseline_lists:except(N, List1, List2)).


-spec get_element(term(), pos_integer(), [tuple()], pos_integer(), term()) -> term().
get_element(Key, N, List, M, Default)
  when ?IS_POS_INTEGER(N), is_list(List), ?IS_POS_INTEGER(M) ->
    case lists:keyfind(Key, N, List) of
        false  ->
            Default;
        Tuple ->
            element(M, Tuple)
    end.


-spec get_value(term(), [tuple()]) -> term().
get_value(Key, List)
  when is_list(List) ->
    get_element(Key, 1, List, 2, undefined).

-spec get_value(term(), [tuple()], term()) -> term().
get_value(Key, List, Default)
  when is_list(List) ->
    get_element(Key, 1, List, 2, Default).

-spec get_value_as_binary(term(), [tuple()], binary()) -> binary().
get_value_as_binary(Key, List, Default)
  when is_list(List), is_binary(Default) ->
    case lists:keyfind(Key, 1, List) of
        false ->
            Default;
        {_, Term} when is_atom(Term) ->
            atom_to_binary(Term, latin1);
        {_, Term}  when is_float(Term) ->
            float_to_binary(Term);
        {_, Term}  when is_integer(Term) ->
            integer_to_binary(Term);
        {_, Term}  when is_list(Term) ->
            list_to_binary(Term)
    end.

-spec get_value_as_boolean(term(), [tuple()], boolean()) -> boolean().
get_value_as_boolean(Key, List, Default)
  when is_list(List), is_boolean(Default) ->
    case lists:keyfind(Key, 1, List) of
        false ->
            Default;
        {_, Term}  when is_atom(Term) ->
            Term =:= true;
        {_, Term}  when is_binary(Term) ->
            Term =:= <<"true">>;
        {_, Term}  when is_float(Term) ->
            Term =/= 0.0;
        {_, Term}  when is_integer(Term) ->
            Term =/= 0;
        {_, Term}  when is_list(Term) ->
            Term =:= "true"
    end.

-spec get_value_as_float(term(), [tuple()], float()) -> float().
get_value_as_float(Key, List, Default)
  when is_list(List), is_float(Default) ->
    case lists:keyfind(Key, 1, List) of
        false ->
            Default;
        {_, Term}  when is_binary(Term) ->
            binary_to_float(Term);
        {_, Term}  when is_list(Term) ->
            list_to_float(Term)
    end.

-spec get_value_as_float(term(), [tuple()], float(), float()) -> float().
get_value_as_float(Key, List, Default, Max)
  when is_list(List), is_float(Default), is_float(Max) ->
    min(get_value_as_float(Key, List, Default), Max).

-spec get_value_as_float(term(), [tuple()], float(), float(), float()) -> float().
get_value_as_float(Key, List, Default, Max, Min)
  when is_list(List), is_float(Default), is_float(Max), is_float(Min), Max >= Min ->
    min(max(get_value_as_float(Key, List, Default), Min), Max).

-spec get_value_as_integer(term(), [tuple()], integer()) -> integer().
get_value_as_integer(Key, List, Default)
  when is_list(List), is_integer(Default) ->
    case lists:keyfind(Key, 1, List) of
        false ->
            Default;
        {_, Term}  when is_binary(Term) ->
            binary_to_integer(Term);
        {_, Term}  when is_list(Term) ->
            list_to_integer(Term)
    end.

-spec get_value_as_integer(term(), [tuple()], integer(), integer()) -> integer().
get_value_as_integer(Key, List, Default, Max)
  when is_list(List), is_integer(Default), is_integer(Max) ->
    min(get_value_as_integer(Key, List, Default), Max).

-spec get_value_as_integer(term(), [tuple()], integer(), integer(), integer()) -> integer().
get_value_as_integer(Key, List, Default, Max, Min)
  when is_list(List), is_integer(Default), is_integer(Max), is_integer(Min), Max >= Min ->
    min(max(get_value_as_integer(Key, List, Default), Min), Max).

-spec get_value_as_list(term(), [tuple()], list()) -> list().
get_value_as_list(Key, List, Default)
  when is_list(List), is_list(Default) ->
    case lists:keyfind(Key, 1, List) of
        false ->
            Default;
        {_, Term}  when is_atom(Term) ->
            atom_to_list(Term);
        {_, Term}  when is_binary(Term) ->
            binary_to_list(Term);
        {_, Term}  when is_float(Term) ->
            float_to_list(Term);
        {_, Term}  when is_integer(Term) ->
            integer_to_list(Term)
    end.
