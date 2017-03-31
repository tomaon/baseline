-module(baseline_binary).

-include("internal.hrl").

%% -- public --
-export([implode/3]).
-export([split/2, split/3]).
-export([prefix/2, suffix/2]).
-export([binary_to_words/3, binary_to_words/4, binary_to_word/3, binary_to_unsigned/4,
         words_to_binary/2, word_to_binary/2, unsigned_to_binary/3]).

%% -- internal --
-define(WORD(N), ((N) * 4)).

-type(pattern() :: binary()|[binary()]|binary:cp()).

%% == public ==

-spec implode(function(), [term()], binary()) -> binary().
implode(Fun, List, Separator)
  when is_function(Fun), is_list(List), is_binary(Separator) ->
    implode(Fun, List, Separator, []).

implode(_Fun, [], _Separator, [_|T]) ->
    list_to_binary(lists:reverse(T));
implode(Fun, [H|T], Separator, List) ->
    implode(Fun, T, Separator, [Separator|[Fun(H)|List]]).


-spec split(binary(), pattern()) -> {[binary()], binary()}.
split(Binary, Pattern) ->
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

-spec split(binary(), pattern(), pattern()) -> [[binary()]].
split(Binary, FieldPattern, LinePattern) ->
    [ binary:split(E, FieldPattern) || E <- binary:split(Binary, LinePattern, [global]) ].


-spec prefix(binary(), binary()) -> boolean().
prefix(Binary1, Binary2)
  when is_binary(Binary1), is_binary(Binary2) ->
    prefix(Binary1, byte_size(Binary1), Binary2, byte_size(Binary2)).

-spec suffix(binary(), binary()) -> boolean().
suffix(Binary1, Binary2)
  when is_binary(Binary1), is_binary(Binary2) ->
    suffix(Binary1, byte_size(Binary1), Binary2, byte_size(Binary2)).


-spec binary_to_words(binary(), non_neg_integer(), endianness()) -> [non_neg_integer()].
binary_to_words(Binary, Start, Endianness) ->
    binary_to_words(Binary, Start, byte_size(Binary), Endianness, []).

-spec binary_to_words(binary(), non_neg_integer(), non_neg_integer(), endianness()) -> [non_neg_integer()].
binary_to_words(Binary, Start, Length,  Endianness)
  when is_binary(Binary), ?IS_NON_NEG_INTEGER(Start), ?IS_NON_NEG_INTEGER(Length), ?IS_ENDIANNESS(Endianness) ->
    binary_to_words(Binary, Start, Length, Endianness, []).

-spec binary_to_word(binary(), non_neg_integer(), endianness()) -> non_neg_integer().
binary_to_word(Binary, Start, Endianness) ->
    binary_to_unsigned(Binary, Start, ?WORD(1), Endianness).

-spec binary_to_unsigned(binary(), non_neg_integer(), non_neg_integer(), endianness()) -> non_neg_integer().
binary_to_unsigned(Binary, Start, Length, little)
  when is_binary(Binary), ?IS_NON_NEG_INTEGER(Start), ?IS_NON_NEG_INTEGER(Length) ->
    <<U:Length/integer-unsigned-little-unit:8>> = binary_part(Binary, Start, Length),
    U;
binary_to_unsigned(Binary, Start, Length, big)
  when is_binary(Binary), ?IS_NON_NEG_INTEGER(Start), ?IS_NON_NEG_INTEGER(Length) ->
    <<U:Length/integer-unsigned-big-unit:8>> = binary_part(Binary, Start, Length),
    U.

-spec words_to_binary([non_neg_integer()], endianness()) -> binary().
words_to_binary(List, Endianness)
  when is_list(List), ?IS_ENDIANNESS(Endianness) ->
    words_to_binary(List, Endianness, []).

-spec word_to_binary(non_neg_integer(), endianness()) -> binary().
word_to_binary(Unsigned, Endianness) ->
    unsigned_to_binary(Unsigned, ?WORD(1), Endianness).

-spec unsigned_to_binary(non_neg_integer(), non_neg_integer(), endianness()) -> binary().
unsigned_to_binary(Unsigned, Size, little)
  when ?IS_NON_NEG_INTEGER(Unsigned), ?IS_NON_NEG_INTEGER(Size)->
    <<Unsigned:Size/integer-unsigned-little-unit:8>>;
unsigned_to_binary(Unsigned, Size, big)
  when ?IS_NON_NEG_INTEGER(Unsigned), ?IS_NON_NEG_INTEGER(Size)->
    <<Unsigned:Size/integer-unsigned-big-unit:8>>.

%% == internal ==

prefix(Binary1, Size1, Binary2, Size2) ->
    Size1 > 0 andalso Size2 > 0
        andalso Size1 >= Size2 andalso Binary2 =:= binary_part(Binary1, 0, Size2).

suffix(Binary1, Size1, Binary2, Size2) ->
    Size1 > 0 andalso Size2 > 0
        andalso Size1 >= Size2 andalso Binary2 =:= binary_part(Binary1, Size1, -Size2).


binary_to_words(_Bianry, _Start, Length, _Endianness, List)
  when Length < ?WORD(1) ->
    lists:reverse(List);
binary_to_words(Binary, Start, Length, Endianness, List) ->
    W = binary_to_word(Binary, Start, Endianness),
    binary_to_words(Binary, Start+?WORD(1), Length-?WORD(1), Endianness, [W|List]).

words_to_binary([], _Endianness, List) ->
    list_to_binary(lists:reverse(List));
words_to_binary([H|T], Endianness, List) ->
    words_to_binary(T, Endianness, [word_to_binary(H, Endianness)|List]).
