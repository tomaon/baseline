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
-export([send/2, recv/3, recv/4]).
-export([call/4]).

%% -- internal --
-record(handle,
        {
          socket :: port(),                     % gen_tcp:socket()
          buf    :: binary()
        }).

-type(handle() :: #handle{}).

%% == private ==

-spec connect(inet:ip_address()|inet:hostname(),inet:port_number(),
              [gen_tcp:connect_option()],timeout()) -> {ok,handle()}|{error,_}.
connect(Address, Port, Options, Timeout) ->
    try gen_tcp:connect(Address, Port, Options, Timeout) of
        {ok, Socket} ->
            {ok, #handle{socket = Socket, buf = <<>>}};
        {error, Reason} ->
            {error, Reason}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec close(handle()) -> ok.
close(#handle{socket=S}) ->
    gen_tcp:close(S).


-spec getopts(handle(),[gen_tcp:option_name()]) -> {ok,[gen_tcp:option()]}|{error,_}.
getopts(#handle{socket=S}, Options) ->
    inet:getopts(S, Options).

-spec setopts(handle(),[gen_tcp:option()]) -> boolean().
setopts(#handle{socket=S}, Options) ->
    ok =:= inet:setopts(S, Options).


-spec getopt_active(handle()) -> {ok,[{active,atom()|integer()}]}.
getopt_active(Handle) ->
    getopts(Handle, [active]).

-spec setopt_active(handle(),atom()|integer()) -> boolean().
setopt_active(Handle, Value) ->
    setopts(Handle, [{active,Value}]).


-spec send(handle(),binary()) -> ok|{error,_}.
send(#handle{socket=S}, Binary) ->
    try gen_tcp:send(S, Binary)
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec recv(handle(),binary:cp()|non_neg_integer(),timeout())
          -> {ok,binary(),handle()}|{error,_,handle()}.
recv(#handle{socket=S,buf=B}=H, Term, Timeout) ->
    case dispatch(S, B, Term, Timeout) of
        {ok, Found, Rest} ->
            {ok, Found, H#handle{buf = Rest}};
        {error, Reason} ->
            {error, Reason, H}
    end.

-spec recv(handle(),binary(),binary:cp()|non_neg_integer(),timeout())
          -> {ok,binary(),handle()}|{error,_,handle()}.
recv(#handle{socket=S,buf=B}=H, Binary, Term, Timeout) ->
    case dispatch(S, <<B/binary,Binary/binary>>, Term, Timeout) of
        {ok, Found, Rest} ->
            {ok, Found, H#handle{buf = Rest}};
        {error, Reason} ->
            {error, Reason, H}
    end.


-spec call(handle(),binary(),binary:cp()|non_neg_integer(),timeout())
          -> {ok,binary(),handle()}|{error,_,handle()}.
call(#handle{}=H, Packet, Term, Timeout) ->
    {ok, Options} = getopt_active(H),
    case setopt_active(H, false) andalso send(H, Packet) of
        ok ->
            case recv(H, Term, Timeout) of
                {ok, Binary, Handle} ->
                    true = setopts(H, Options),
                    {ok, Binary, Handle};
                {error, Reason, Handle} ->
                    true = setopts(Handle, Options),
                    {error, Reason, Handle}
            end;
        {error, Reason} ->
            true = setopts(H, Options),
            {error, Reason, H}
    end.

%% == internal ==

dispatch(Socket, Binary, Term, Timeout)
  when is_integer(Term) ->
    recv_binary(Socket, Binary, Term, Timeout);
dispatch(Socket, Binary, Term, Timeout) ->
    recv_text(Socket, Binary, Term, Timeout).

recv_binary(Socket, Binary, Size, Timeout) ->
    recv_binary(Socket, Binary, Size, Timeout, size(Binary) >= Size).

recv_binary(_Socket, Binary, Size, _Timeout, true) ->
    erlang:insert_element(1, split_binary(Binary,Size), ok);
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
    {ok, binary_part(Binary,0,S), binary_part(Binary,S+L,size(Binary)-(S+L))};
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
