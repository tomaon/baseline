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

-module(baseline_socket).

-include("internal.hrl").

%% -- private --
-export([connect/4, close/1]).
-export([getopts/2, setopts/2]).
-export([getopt_active/1, setopt_active/2]).
-export([send/2, recv/3]).
-export([call/4]).

%% -- internal --
-record(socket,
        {
          port       :: port(),                 % gen_tcp:socket()
          buf = <<>> :: binary()
        }).

-type(socket() :: #socket{}).

%% == private ==

-spec connect(inet:ip_address()|inet:hostname(),inet:port_number(),
              [gen_tcp:connect_option()],timeout()) -> {ok,socket()}|{error,_}.
connect(Address, Port, Options, Timeout) ->
    try gen_tcp:connect(Address, Port, Options, Timeout) of
        {ok, Socket} ->
            {ok, #socket{port = Socket}};
        {error, Reason} ->
            {error, Reason}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec close(socket()) -> ok.
close(#socket{port=P}) ->
    gen_tcp:close(P).


-spec getopts(socket(),[gen_tcp:option_name()]) -> {ok,[gen_tcp:option()]}|{error,_}.
getopts(#socket{port=P}, Options) ->
    inet:getopts(P, Options).

-spec setopts(socket(),[gen_tcp:option()]) -> boolean().
setopts(#socket{port=P}, Options) ->
    ok =:= inet:setopts(P, Options).


-spec getopt_active(socket()) -> {ok,[{active,atom()|integer()}]}.
getopt_active(Socket) ->
    getopts(Socket, [active]).

-spec setopt_active(socket(),atom()|integer()) -> boolean().
setopt_active(Socket, Value) ->
    setopts(Socket, [{active,Value}]).


-spec send(socket(),binary()) -> ok|{error,_}.
send(#socket{port=P}, Binary) ->
    try gen_tcp:send(P, Binary)
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec recv(socket(),binary:cp()|non_neg_integer(),timeout())
          -> {ok,binary(),socket()}|{error,_}.
recv(#socket{port=P,buf=B}=S, Term, Timeout) ->
    case dispatch(P, B, Term, Timeout) of
        {ok, Found, Rest} ->
            {ok, Found, S#socket{buf = Rest}};
        {error, Reason} ->
            {error, Reason}
    end.


-spec call(socket(),binary(),binary:cp()|non_neg_integer(),timeout())
          -> {ok,binary(),socket()}|{error,_}.
call(#socket{}=S, Packet, Term, Timeout) ->
    case send(S, Packet) of
        ok ->
            case recv(S, Term, Timeout) of
                {ok, Binary, Socket} ->
                    {ok, Binary, Socket};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% == internal ==

dispatch(Socket, Binary, Term, Timeout)
  when is_integer(Term) ->
    recv_binary(Socket, Binary, Term, Timeout);
dispatch(Socket, Binary, Term, Timeout) ->
    recv_text(Socket, Binary, Term, Timeout).

recv_binary(Socket, Binary, Size, Timeout) ->
    recv_binary(Socket, Binary, Size, Timeout, byte_size(Binary) >= Size).

recv_binary(_Socket, Binary, Size, _Timeout, true) ->
    {ok, binary_part(Binary,0,Size), binary_part(Binary,Size,byte_size(Binary)-Size)};
recv_binary(Socket, Binary, Size, Timeout, false) ->
    try gen_tcp:recv(Socket, 0, Timeout) of
        {ok, Packet} ->
            recv_binary(Socket, <<Binary/binary,Packet/binary>>, Size, Timeout);
        {error, Reason} ->
            {error, Reason}
    catch
        {error, Reason} ->
            {error, Reason}
    end.

recv_text(Socket, Binary, Pattern, Timeout) ->
    recv_text(Socket, Binary, Pattern, Timeout, binary:match(Binary,Pattern)).

recv_text(_Socket, Binary, _Pattern, _Timeout, {S,L}) ->
    {ok, binary_part(Binary,0,S), binary_part(Binary,S+L,byte_size(Binary)-(S+L))};
recv_text(Socket, Binary, Pattern, Timeout, nomatch) ->
    try gen_tcp:recv(Socket, 0, Timeout) of
        {ok, Packet} ->
            recv_text(Socket, <<Binary/binary,Packet/binary>>, Pattern, Timeout);
        {error, Reason} ->
            {error, Reason}
    catch
        _:Reason ->
            {error, Reason}
    end.
