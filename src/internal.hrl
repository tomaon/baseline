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

-record(baseline_drv, {
          path :: string(),
          name :: string(),
          settings :: [property()]
         }).

-type(baseline_drv() :: #baseline_drv{}).


-type filename() :: file:filename().
-type property() :: proplists:property().
-type startchild_ret() :: {ok,pid()}|{ok,pid(),term()}|{error,_}.
-type startlink_ret() :: {ok,pid()}|ignore|{error,_}.
-type stop_ret() :: ok.
-type sup_name() :: {local,atom()}|{global,atom()}.
-type sup_ref() :: atom()|{atom(),node()}|{global,atom()}|pid().
-type terminatechild_ret() :: ok|{error,_}.
