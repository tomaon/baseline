%% =============================================================================
%% Copyright 2014 AONO Tomohiko
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

%% == public ==

-spec choose([term()]) -> term()|undefined.
choose(List)
  when is_list(List) ->
    choose(List, 2).

-spec choose([term()], integer()) -> term()|undefined.
choose([], _N) ->
    undefined;
choose(List, N)
  when is_list(List), is_integer(N) ->
    lists:nth(1 + binary:decode_unsigned(crypto:rand_bytes(N)) rem length(List), List).

-spec equalize(pos_integer(),pos_integer()) -> [pos_integer()]|{error,_}.
equalize(N, W)
  when is_integer(N), N > 0, is_integer(W), W > 0 ->
    equalize(min(N,W), 0, N div W, N rem W, []);
equalize(_N, _W) ->
    {error, badarg}.

equalize(0, _Sum, _Div, _Rem, List) ->
    lists:reverse(List);
equalize(Num, Sum, Div, 0, List) ->
    E = Sum + Div,
    equalize(Num - 1, E, Div, 0, [E|List]);
equalize(Num, Sum, Div, Rem, List) ->
    E = Sum + Div + 1,
    equalize(Num - 1, E, Div, Rem - 1, [E|List]).

-spec except([property()],[property()]) -> [property()].
except(List1, List2)
  when is_list(List1), is_list(List2) ->
    Keys = proplists:get_keys(List1),
    lists:filter(fun({K,_}) -> not(lists:member(K,Keys)) end, proplists:unfold(List2)).

-spec merge([property()],[property()]) -> [property()].
merge(List1, List2)
  when is_list(List1), is_list(List2) ->
    List1 ++ baseline_lists:except(List1, List2).
