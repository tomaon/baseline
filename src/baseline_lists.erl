%% =============================================================================
%% Copyright 2014-2015 AONO Tomohiko
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License version 2.1 as published by the Free Software Foundation.
%%
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%% =============================================================================

-module(baseline_lists).

-include("internal.hrl").

%% -- public --
-export([choose/1, choose/2]).
-export([equalize/2]).
-export([except/2]).
-export([merge/2]).
-export([combine/3, combine/4]).
-export([get_as_binary/4,
         get_as_integer/4, get_as_integer/5, get_as_integer/6,
         get_as_list/4]).

%% == public ==

-spec choose([term()]) -> term()|undefined.
choose(List) ->
    choose(List, 2).

-spec choose([term()], integer()) -> term()|undefined.
choose([], _Size) ->
    undefined;
choose(List, Size)
  when is_list(List), is_integer(Size) ->
    lists:nth(1 + binary:decode_unsigned(crypto:rand_bytes(Size)) rem length(List), List).


-spec equalize(pos_integer(),pos_integer()) -> [pos_integer()].
equalize(N, W)
  when ?IS_POS_INTEGER(N), ?IS_POS_INTEGER(W) ->
    equalize(min(N,W), 0, N div W, N rem W, []).

equalize(0, _Sum, _Div, _Rem, List) ->
    lists:reverse(List);
equalize(Num, Sum, Div, 0, List) ->
    E = Sum + Div,
    equalize(Num - 1, E, Div, 0, [E|List]);
equalize(Num, Sum, Div, Rem, List) ->
    E = Sum + Div + 1,
    equalize(Num - 1, E, Div, Rem - 1, [E|List]).


-spec except(proplists:proplist(),proplists:proplist()) -> proplists:proplist().
except(List1, List2)
  when is_list(List1), is_list(List2) ->
    F = fun (E) ->
                K = if is_tuple(E) -> element(1, E);
                       true -> E
                    end,
                not(proplists:is_defined(K,List2))
        end,
    lists:filter(F, List1).


-spec merge(proplists:proplist(),proplists:proplist()) -> proplists:proplist().
merge(List1, List2)
  when is_list(List1), is_list(List2) ->
    lists:sort(List1 ++ baseline_lists:except(List2, List1)).


-spec combine(pos_integer(),[tuple()],[tuple()]) -> [tuple()].
combine(N, List1, List2) ->
    combine(N, List1, List2, []).

-spec combine(pos_integer(),[tuple()],[tuple()],[term()]) -> [tuple()].
combine(N, List1, List2, Excludes)
  when ?IS_POS_INTEGER(N), is_list(List1), is_list(List2), is_list(Excludes) ->
    lists:filtermap(fun ({K,V}) ->
                            case lists:keyfind(K, N, List2) of
                                {K, A} ->
                                    {true, {A,V}};
                                false ->
                                    case lists:member(K, Excludes) of
                                        false ->
                                            {true, {K,V}};
                                        true ->
                                            false
                                    end
                            end
                    end, List1).


-spec get_as_binary(term(),pos_integer(),[tuple()],binary()) -> binary().
get_as_binary(Key, N, List, DefaultValue)
  when ?IS_POS_INTEGER(N), is_list(List), is_binary(DefaultValue) ->
    case lists:keyfind(Key, N, List) of
        {Key, Term} when is_binary(Term) -> Term;
        {Key, Term} when is_integer(Term) -> integer_to_binary(Term);
        {Key, Term} when is_list(Term) -> list_to_binary(Term);
        false -> DefaultValue
    end.

-spec get_as_integer(term(),pos_integer(),[tuple()],integer()) -> integer().
get_as_integer(Key, N, List, DefaultValue)
  when ?IS_POS_INTEGER(N), is_list(List), is_integer(DefaultValue) ->
    case lists:keyfind(Key, N, List) of
        {Key, Term} when is_integer(Term) -> Term;
        {Key, Term} when is_binary(Term) -> binary_to_integer(Term);
        {Key, Term} when is_list(Term) -> list_to_integer(Term);
        false -> DefaultValue
    end.

-spec get_as_integer(term(),pos_integer(),[tuple()],integer(),integer()) -> integer().
get_as_integer(Key, N, List, DefaultValue, Max)
  when ?IS_POS_INTEGER(N), is_list(List), is_integer(DefaultValue), is_integer(Max) ->
    min(get_as_integer(Key,N,List,DefaultValue), Max).

-spec get_as_integer(term(),pos_integer(),[tuple()],integer(),integer(),integer()) -> integer().
get_as_integer(Key, N, List, DefaultValue, Max, Min)
  when ?IS_POS_INTEGER(N), is_list(List), is_integer(DefaultValue), is_integer(Max), is_integer(Min), Max >= Min ->
    min(max(get_as_integer(Key,N,List,DefaultValue),Min), Max).

-spec get_as_list(term(),pos_integer(),[tuple()],list()) -> list().
get_as_list(Key, N, List, DefaultValue)
  when ?IS_POS_INTEGER(N), is_list(List), is_list(DefaultValue) ->
    case lists:keyfind(Key, N, List) of
        {Key, Term} when is_list(Term) -> Term;
        {Key, Term} when is_binary(Term) -> binary_to_list(Term);
        {Key, Term} when is_integer(Term) -> integer_to_list(Term);
        false -> DefaultValue
    end.
