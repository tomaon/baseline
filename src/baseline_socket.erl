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
-export([unshift/2]).

%% -- internal --
-record(socket,
        {
          port          :: port(),              % gen_tcp:socket()
          buf    = <<>> :: binary(),
          start  = 0    :: non_neg_integer(),
          length = 0    :: non_neg_integer()
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

-spec recv(socket(),non_neg_integer()|binary()|binary:cp(),timeout())
          -> {ok,binary(),socket()}|{error,_,socket()}.
recv(#socket{}=R, Term, Timeout)
  when is_integer(Term) ->
    recv_binary(R, Term, Timeout);
recv(#socket{}=R, Term, Timeout) ->
    recv_text(R, Term, Timeout).

-spec recv_binary(socket(),non_neg_integer(),timeout())
                 -> {ok,binary(),socket()}|{error,_,socket()}.
recv_binary(#socket{length=L}=R, Length, Timeout) ->
    recv_binary(R, Length, Timeout, L >= Length).

-spec recv_text(socket(),binary()|binary:cp(),timeout())
               -> {ok,binary(),socket()}|{error,_,socket()}.
recv_text(#socket{buf=B,start=S,length=L}=R, Pattern, Timeout) ->
    Binary = binary_part(B, S, L),
    recv_text(R, Pattern, Timeout, Binary, L, binary:match(Binary,Pattern)).


-spec call(socket(),non_neg_integer()|binary(),binary:cp(),timeout())
          -> {ok,binary(),socket()}|{error,_,socket()}.
call(#socket{}=R, Packet, Term, Timeout) ->
    {ok, [{active,V}]} = getopt_active(R),
    try setopt_active(R, false) andalso send(R, Packet) of
        ok ->
            recv(R, Term, Timeout);
        {error, Reason} ->
            {error, Reason, R}
    after
        setopt_active(R, V)
    end.


-spec unshift(socket(),binary()) -> socket().
unshift(#socket{buf=B,start=S,length=L}=R, Binary) ->
    X = binary_part(B, S, L),
    R#socket{buf = <<X/binary,Binary/binary>>, start = 0, length = L+byte_size(Binary)}.

%% == internal ==

update(#socket{port=P}=R, Binary, Length, Timeout) ->
    try gen_tcp:recv(P, 0, Timeout) of
        {ok, Packet} ->
            {ok, R#socket{buf = <<Binary/binary,Packet/binary>>,
                          start = 0, length = Length + byte_size(Packet)}};
        {error, Reason} ->
            {error, Reason}
    catch
        _:Reason ->
            {error, Reason}
    end.


recv_binary(#socket{buf=B,start=S,length=L}=R, Length, _Timeout, true) ->
    {ok, binary_part(B,S,Length), R#socket{start = S+Length, length = L-Length}};
recv_binary(#socket{buf=B,start=S,length=L}=R, Length, Timeout, false) ->
    case update(R, binary_part(B,S,L), L, Timeout) of
        {ok, Socket} ->
            recv_binary(Socket, Length, Timeout);
        {error, Reason} ->
            {error, Reason, R}
    end.

recv_text(#socket{start=S}=R, _Pattern, _Timeout, Binary, Length, {MS,ML}) ->
    {ok, binary_part(Binary,0,MS), R#socket{start = S+(MS+ML), length = Length-(MS+ML)}};
recv_text(#socket{}=R, Pattern, Timeout, Binary, Length, nomatch) ->
    case update(R, Binary, Length, Timeout) of
        {ok, Socket} ->
            recv_text(Socket, Pattern, Timeout);
        {error, Reason} ->
            {error, Reason, R}
    end.
