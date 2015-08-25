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

-module(baseline_binary).

-include("internal.hrl").

%% -- public --
-export([prefix/2, suffix/2]).
-export([binary_to_word/3]).

%% == public ==

-spec prefix(binary(),binary()) -> boolean().
prefix(Binary1, Binary2)
  when is_binary(Binary1), is_binary(Binary2) ->
    prefix(Binary1, byte_size(Binary1), Binary2, byte_size(Binary2)).

-spec suffix(binary(),binary()) -> boolean().
suffix(Binary1, Binary2)
  when is_binary(Binary1), is_binary(Binary2) ->
    suffix(Binary1, byte_size(Binary1), Binary2, byte_size(Binary2)).


-spec binary_to_word(binary(),non_neg_integer(),endianness()) -> non_neg_integer().
binary_to_word(Binary, Start, Endianness)
  when is_binary(Binary), ?IS_NON_NEG_INTEGER(Start), ?IS_ENDIANNESS(Endianness) ->
    binary_to_word(Binary, Start, 4, Endianness).

%% == internal ==

prefix(Binary1, Size1, Binary2, Size2) ->
    Size1 > 0 andalso Size2 > 0
        andalso Size1 >= Size2 andalso Binary2 =:= binary_part(Binary1, {0,Size2}).

suffix(Binary1, Size1, Binary2, Size2) ->
    Size1 > 0 andalso Size2 > 0
        andalso Size1 >= Size2 andalso Binary2 =:= binary_part(Binary1, {Size1,-Size2}).


binary_to_word(Binary, Start, Length, little) ->
    <<W:Length/integer-unsigned-little-unit:8>> = binary_part(Binary, {Start,Length}),
    W;
binary_to_word(Binary, Start, Length, big) ->
    <<W:Length/integer-unsigned-big-unit:8>> = binary_part(Binary, {Start,Length}),
    W.
