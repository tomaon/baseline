-module(baseline_binary).

-include("internal.hrl").

%% -- public --
-export([decode_unsigned/3, decode_unsigned/4, decode_unsigned/5,
         encode_unsigned/2, encode_unsigned/3]).
-export([implode/3]).
-export([split/2]).

%% -- internal --
-define(IS_PATTERN(T), (true)).

-type(pattern() :: binary()|[binary()]|binary:cp()).

%% == public ==

-spec decode_unsigned(binary(), non_neg_integer(), integer())
                     -> non_neg_integer().
decode_unsigned(Subject, Start, Length) ->
    decode_unsigned(Subject, Start, Length, big).

-spec decode_unsigned(binary(), non_neg_integer(), integer(), endianness())
                     -> non_neg_integer().
decode_unsigned(Subject, Start, Length, Endianness)
  when is_binary(Subject), ?IS_NON_NEG_INTEGER(Start),
       is_integer(Length), ?IS_ENDIANNESS(Endianness) ->
    binary:decode_unsigned(binary_part(Subject, Start, Length), Endianness).

-spec decode_unsigned(binary(), non_neg_integer(), pos_integer(), endianness(), pos_integer())
                     -> [non_neg_integer()].
decode_unsigned(Subject, Start, Length, Endianness, Incr)
  when is_binary(Subject), ?IS_NON_NEG_INTEGER(Start),
       ?IS_POS_INTEGER(Length), ?IS_ENDIANNESS(Endianness), ?IS_POS_INTEGER(Incr) ->
    decode_unsigned(Subject, Start, Length, Endianness, Incr, []).

decode_unsigned(_Subject, _Start, Length, _Endianness, Incr, List)
  when Length < Incr ->
    lists:reverse(List);
decode_unsigned(Subject, Start, Length, Endianness, Incr, List) ->
    E = decode_unsigned(Subject, Start, Incr, Endianness),
    decode_unsigned(Subject, Start + Incr, Length - Incr, Endianness, Incr, [E|List]).

-spec encode_unsigned(non_neg_integer(), pos_integer()) -> binary().
encode_unsigned(Unsigned, Width) ->
    encode_unsigned(Unsigned, Width, big).

-spec encode_unsigned(non_neg_integer(), pos_integer(), endianness()) -> binary().
encode_unsigned(Unsigned, Width, big)
  when ?IS_NON_NEG_INTEGER(Unsigned), ?IS_POS_INTEGER(Width) ->
    <<Unsigned:Width/integer-unsigned-big-unit:8>>;
encode_unsigned(Unsigned, Width, little)
  when ?IS_NON_NEG_INTEGER(Unsigned), ?IS_POS_INTEGER(Width) ->
    <<Unsigned:Width/integer-unsigned-little-unit:8>>.


-spec implode(function(), [term()], binary()) -> binary().
implode(Fun, List, Separator)
  when is_function(Fun), is_list(List), is_binary(Separator) ->
    implode(Fun, List, Separator, []).

implode(_Fun, [], _Separator, [_|T]) ->
    list_to_binary(lists:reverse(T));
implode(Fun, [H|T], Separator, List) ->
    implode(Fun, T, Separator, [Separator|[Fun(H)|List]]).


-spec split(binary(), pattern()) -> {[binary()], binary()}.
split(Binary, Pattern)
  when is_binary(Binary), ?IS_PATTERN(Pattern) ->
    split(Binary, 0, [], binary:matches(Binary, Pattern)).

split(Binary, Start, List, []) ->
    {lists:reverse(List), binary_part(Binary, Start, size(Binary) - Start)};
split(Binary, Start, List, [{S, L}|T]) ->
    case binary_part(Binary, Start, S - Start) of
        <<>> ->
            split(Binary, S + L, List, T);
        Part ->
            split(Binary, S + L, [Part|List], T)
    end.
