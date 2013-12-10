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

-module(eroonga_driver).

-include("eroonga_internal.hrl").

%% -- public --
-export([load/1, unload/1]).
-export([open/1, open/2, close/1]).
-export([call/3, command/2, control/3]).

%% -- private --
-record(inner, {
          path :: string(),
          name :: string(),
          settings = [] :: [property()]
         }).

-type(inner() :: #inner{}).

%% == public ==

-spec load([property()]) -> {ok,inner()}|{error,_}.
load(Configs)
  when is_list(Configs) ->
    try lists:foldl(fun check_config/2, #inner{}, Configs ++ [{path,undefined}]) of
        #inner{path=P,name=N}=R ->
            case erl_ddll:load(P, N) of
                ok ->
                    {ok, R};
                {error, already_loaded} ->
                    {ok, R};
                {error, Reason} ->
                    error_logger:error_report([erl_ddll:format_error(Reason),R]),
                    {error, Reason}
            end
    catch
        Reason ->
            {error, Reason}
    end.

-spec unload(inner()) -> ok|{error,_}.
unload(#inner{name=N}) ->
    erl_ddll:unload(N).

-spec open(inner()) -> {ok,port()}|{error,_}.
open(#inner{settings=L}=R) ->
    open(R, [binary|proplists:delete(binary,L)]).

-spec open(inner(),[property()]) -> {ok,port()}|{error,_}.
open(#inner{name=N}, Settings)
  when is_list(Settings) ->
    try open_port({spawn_driver,N}, Settings) of
        Port ->
            {ok, Port}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec close(port()) -> ok.
close(Port)
  when is_port(Port)->
    unlink(Port),
    port_close(Port),
    ok.

-spec call(port(),integer(),any()) -> ok|{error,_}.
call(Port, Command, Data)
  when is_port(Port), is_integer(Command), is_binary(Data) ->
    control(Port, Command, Data); % NOT_SUPPORTED?, darwin-64bit (12.2.0)
call(Port, Command, Data)
  when is_port(Port) ->
    control(Port, Command, term_to_binary(Data)).

-spec command(port(),binary()) -> ok|{error,_}.
command(Port, Data)
  when is_port(Port), is_binary(Data) ->
    try port_command(Port, Data) of
        true ->
            ok
    catch
        _:Reason ->
            {error, Reason}
    end;
command(Port, Data)
  when is_port(Port) ->
    command(Port, term_to_binary(Data)).

-spec control(port(),integer(),any()) -> ok|{error,_}.
control(Port, Command, Data)
  when is_port(Port), is_integer(Command), is_binary(Data) ->
    try port_control(Port, Command, Data) of
        Term when is_binary(Term) ->
            binary_to_term(Term)
    catch
        _:Reason ->
            {error, Reason}
    end;
control(Port, Command, Data)
  when is_port(Port), is_integer(Command) ->
    control(Port, Command, term_to_binary(Data)).

%% == private ==

-spec check_config(property(),inner()) -> inner().
%% -- path --
check_config({path, Path}, #inner{path=undefined}=R)
  when is_atom(Path) ->
    R#inner{path = lib_dir(Path)};
check_config({path, Path}, #inner{path=undefined}=R)
  when is_binary(Path) ->
    R#inner{path = binary_to_list(Path)};
check_config({path, Path}, #inner{path=undefined}=R)
  when is_list(Path) ->
    R#inner{path = Path};
%% -- name --
check_config({name, Name}, #inner{name=undefined}=R)
  when is_atom(Name) ->
    R#inner{name = atom_to_list(Name)};
check_config({name, Name}, #inner{name=undefined}=R)
  when is_binary(Name) ->
    R#inner{name = binary_to_list(Name)};
check_config({name, Name}, #inner{name=undefined}=R)
  when is_list(Name) ->
    R#inner{name = Name};
%% -- settings --
check_config({settings, Settings}, #inner{name=undefined}=R)
  when is_list(Settings) ->
    R#inner{settings = Settings};
%% -- --
check_config({Key, _Value}, #inner{}) ->
    throw({badmatch, Key}).

-spec lib_dir(atom()) -> filename().
lib_dir(Application) ->
    filename:join(lib_dir(Application, priv), "lib").

-spec lib_dir(atom(),atom()) -> filename().
lib_dir(Application, SubDir) ->
    case code:lib_dir(Application, SubDir) of
        {error, bad_name} ->
            {ok, Dir} = file:get_cwd(),
            filename:join(Dir, atom_to_list(SubDir));
        Dir ->
            Dir
    end.
