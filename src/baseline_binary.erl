-module(baseline_binary).

-include("internal.hrl").

%% -- public --
-export([decode_signed/2, decode_signed/4, decode_signed/5,
         decode_unsigned/2, decode_unsigned/4, decode_unsigned/5]).
-export([encode_signed/3, encode_unsigned/3]).
-export([fold/6]).
-export([implode/3]).
-export([split/2]).

%% -- internal --
-define(IS_PATTERN(T), (true)). % TODO

-type(pattern() :: binary()|[binary()]|binary:cp()).

%% == public ==

-spec decode_signed(binary(), endianness()) -> integer().
decode_signed(Subject, Endianness)
  when is_binary(Subject), ?IS_ENDIANNESS(Endianness) ->
    decode_signed(Subject, size(Subject), Endianness).

decode_signed(Subject, Size, little) -> <<I:Size/signed-little-unit:8>> = Subject, I;
decode_signed(Subject, Size, native) -> <<I:Size/signed-native-unit:8>> = Subject, I;
decode_signed(Subject, Size, big) -> <<I:Size/signed-big-unit:8>> = Subject, I.

-spec decode_signed(binary(), non_neg_integer(), integer(), endianness())
                   -> integer().
decode_signed(Subject, Start, Length, Endianness)
  when is_binary(Subject), ?IS_NON_NEG_INTEGER(Start),
       is_integer(Length), ?IS_ENDIANNESS(Endianness) ->
    decode_signed(binary_part(Subject, Start, Length), abs(Length), Endianness).

-spec decode_signed(binary(), non_neg_integer(), non_neg_integer(), integer(), endianness())
                   -> [integer()].
decode_signed(Subject, Start, Size, Incr, Endianness)
  when is_binary(Subject), ?IS_NON_NEG_INTEGER(Start),
       ?IS_NON_NEG_INTEGER(Size), is_integer(Incr), ?IS_ENDIANNESS(Endianness) ->
    N = abs(Incr),
    lists:reverse(fold(fun(E, A) -> [decode_signed(E, N, Endianness)|A] end,
                       [], Subject, Start, Size, Incr)).

-spec decode_unsigned(binary(), endianness()) -> non_neg_integer().
decode_unsigned(Subject, Endianness)
  when is_binary(Subject), ?IS_ENDIANNESS(Endianness) ->
    decode_unsigned(Subject, size(Subject), Endianness).

decode_unsigned(Subject, Size, little) -> <<I:Size/unsigned-little-unit:8>> = Subject, I;
decode_unsigned(Subject, Size, native) -> <<I:Size/unsigned-native-unit:8>> = Subject, I;
decode_unsigned(Subject, Size, big) -> <<I:Size/unsigned-big-unit:8>> = Subject, I.

-spec decode_unsigned(binary(), non_neg_integer(), integer(), endianness())
                     -> non_neg_integer().
decode_unsigned(Subject, Start, Length, Endianness)
  when is_binary(Subject), ?IS_NON_NEG_INTEGER(Start),
       is_integer(Length), ?IS_ENDIANNESS(Endianness) ->
    decode_unsigned(binary_part(Subject, Start, Length), abs(Length), Endianness).

-spec decode_unsigned(binary(), non_neg_integer(), non_neg_integer(), integer(), endianness())
                     -> [non_neg_integer()].
decode_unsigned(Subject, Start, Size, Incr, Endianness)
  when is_binary(Subject), ?IS_NON_NEG_INTEGER(Start),
       ?IS_NON_NEG_INTEGER(Size), is_integer(Incr), ?IS_ENDIANNESS(Endianness) ->
    N = abs(Incr),
    lists:reverse(fold(fun(E, A) -> [decode_unsigned(E, N, Endianness)|A] end,
                       [], Subject, Start, Size, Incr)).

-spec encode_signed(integer(), pos_integer(), endianness()) -> binary().
encode_signed(Signed, Size, little)
  when is_integer(Signed), ?IS_POS_INTEGER(Size) ->
    <<Signed:Size/signed-little-unit:8>>;
encode_signed(Signed, Size, native)
  when is_integer(Signed), ?IS_POS_INTEGER(Size) ->
    <<Signed:Size/signed-native-unit:8>>;
encode_signed(Signed, Size, big)
  when is_integer(Signed), ?IS_POS_INTEGER(Size) ->
    <<Signed:Size/signed-big-unit:8>>.

-spec encode_unsigned(non_neg_integer(), pos_integer(), endianness()) -> binary().
encode_unsigned(Unsigned, Size, little)
  when ?IS_NON_NEG_INTEGER(Unsigned), ?IS_POS_INTEGER(Size) ->
    <<Unsigned:Size/unsigned-little-unit:8>>;
encode_unsigned(Unsigned, Size, native)
  when ?IS_NON_NEG_INTEGER(Unsigned), ?IS_POS_INTEGER(Size) ->
    <<Unsigned:Size/unsigned-native-unit:8>>;
encode_unsigned(Unsigned, Size, big)
  when ?IS_NON_NEG_INTEGER(Unsigned), ?IS_POS_INTEGER(Size) ->
    <<Unsigned:Size/unsigned-big-unit:8>>.


-spec fold(function(), term(), binary(), non_neg_integer(), non_neg_integer(), integer())
          -> term().
fold(Fun, Acc, Subject, Start, Size, Incr)
  when is_function(Fun), is_binary(Subject),
       ?IS_NON_NEG_INTEGER(Start), ?IS_NON_NEG_INTEGER(Size), is_integer(Incr) ->
    fold(Fun, Acc, Subject, Start, Size, Incr, abs(Incr)).

fold(_Fun, Acc, _Subject, _Start, Size, _Incr, N)
  when Size < N ->
    Acc;
fold(Fun, Acc, Subject, Start, Size, Incr, N) ->
    E = binary_part(Subject, Start, Incr),
    fold(Fun, Fun(E, Acc), Subject, Start + Incr, Size - N, Incr, N).


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
