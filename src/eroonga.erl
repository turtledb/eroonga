%% =============================================================================
%% Copyright 2013 AONO Tomohiko
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% =============================================================================

-module(eroonga).

%% -- public: application --
-export([start/0, stop/0, get_client_version/0]).

%% -- public: pool --
-export([connect/1, close/2]).

%% -- public: worker --
-export([command/2]).

-export([status/1, table_list/1]).

%% == public: application ==

-spec start() -> ok|{error,_}.
start() ->
    ok = lists:foreach(fun application:start/1, eroonga_app:deps()),
    application:start(?MODULE).

-spec stop() -> ok|{error,_}.
stop() ->
    application:stop(?MODULE).

-spec get_client_version() -> {ok,[non_neg_integer()]}.
get_client_version() ->
    {ok, eroonga_app:version()}.

%% == public: pool ==

-spec connect(atom()) -> {ok,pid()}|{error,_}.
connect(Pool)
  when is_atom(Pool) ->
    connect(Pool, false).

-spec connect(atom(),boolean()) -> {ok,pid()}|{error,_}.
connect(Pool, Block)
  when is_atom(Pool), is_boolean(Block) ->
    connect(Pool, Block, timer:seconds(5)).

-spec connect(atom(),boolean(),timeout()) -> {ok,pid()}|{error,_}.
connect(Pool, Block, Timeout)
  when is_atom(Pool), is_boolean(Block) ->
    eroonga_app:checkout(Pool, Block, Timeout).

-spec close(atom(),pid()) -> ok|{error,_}.
close(Pool, Worker)
  when is_atom(Pool), is_pid(Worker) ->
    eroonga_app:checkin(Pool, Worker).

%% == public: worker ==

-spec command(pid(),binary()) -> {ok,term()}|{error,_}.
command(Pid, Binary)
  when is_pid(Pid), is_binary(Binary) ->
    eroonga_client:call(Pid, {call,[Binary]}).


-spec status(pid()) -> {ok,term()}|{error,_}.
status(Pid)
  when is_pid(Pid) ->
    command(Pid, <<"status">>).

-spec table_list(pid()) -> {ok,term()}|{error,_}.
table_list(Pid)
  when is_pid(Pid) ->
    command(Pid, <<"table_list">>).

%% TODO
%%  cache_limit, check, clearlock, column_create, column_list, column_remove,
%%  column_rename, define_selector, defrag, delete, dump, load, log_level,
%%  log_put, log_reopen, normalize, quit, register, ruby_eval, ruby_load,
%%  select, shutdown, suggest, table_create, table_remove, tokenize,truncate
