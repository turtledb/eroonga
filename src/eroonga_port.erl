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

-module(eroonga_port).

-include("eroonga_internal.hrl").

%% -- public --
-export([start_link/3, stop/1]).
-export([call/3, call/4]).

%% -- behaviour: gen_server --
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% == public ==

-spec start_link(tuple(),[any()],pos_integer()) -> {ok,pid()}|{error,_}.
start_link(Driver, Args, Id)
  when is_tuple(Driver), is_list(Args), is_integer(Id), Id > 0 ->
    case gen_server:start_link(?MODULE, [Id], []) of
        {ok, Pid} ->
            case gen_server:call(Pid, {setup,Driver,Args}) of
                ok ->
                    {ok, Pid};
                {error, Reason} ->
                    stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(pid()) -> ok.
stop(Pid)
  when is_pid(Pid) ->
    gen_server:call(Pid, stop).

-spec call(pid(),integer(),any()) -> any().
call(Pid, Command, Args)
  when is_pid(Pid), is_integer(Command) ->
    gen_server:call(Pid, {call,Command,Args}).

-spec call(pid(),integer(),any(),function()) -> [any()]|{error,_}.
call(Pid, Command, Args, Fun)
  when is_pid(Pid), is_integer(Command), is_function(Fun) ->
    gen_server:call(Pid, {call,Command,Args,Fun}).

%% == behaviour: gen_server ==

-record(inner, {
          from :: {pid(),_},
          handler :: function(),
          list = [] :: [any()]
         }).

-type(inner() :: #inner{}).

-record(state, { % 1-client = 1-id = 1-port
          id  :: non_neg_integer(),
          port :: port(),
          command = 0 :: integer(),
          assigned :: inner()
         }).

-type(state() :: #state{}).

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({call,Command,Args}, _From, #state{port=P,assigned=undefined}=S) ->
    Result = ergen_driver:call(P, Command, Args),
    {reply, Result, S};
handle_call({call,Command,Args,Fun}, From, #state{port=P,assigned=undefined}=S) ->
    case ergen_driver:command(P, {Command,Args}) of
        ok ->
            {noreply, S#state{assigned = #inner{from = From, handler = Fun} }};
        {error, Reason} ->
            {reply, {error,Reason}, S}
    end;
handle_call({setup,Driver,Args}, _From, #state{}=S) ->
    case setup(Driver, Args, S) of
        {ok, State} ->
            {reply, ok, State};
        {error, Reason, State} ->
            {reply, {error,Reason}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error,badarg}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({P,{data,_}}, #state{port=P,assigned=undefined}=S) ->
    {noreply, S};
handle_info({P,{data,Data}}, #state{port=P,assigned=A}=S) ->
    #inner{from=F,handler=H,list=L} = A,
    case binary_to_term(Data) of
        {ok, Term} ->
            case H(Term) of
                ignore ->
                    {noreply, S};
                {noreply, Result} ->
                    {noreply, S#state{assigned = A#inner{list = [Result|L]} }};
                {reply, Result} ->
                    gen_server:reply(F, lists:reverse([Result|L])),
                    {noreply, S#state{assigned = undefined}}
            end;
        {error, Reason} ->
            gen_server:reply(F, {error,Reason}),
            {noreply, S#state{assigned = undefined}}
    end;
handle_info({'EXIT',P,Reason}, #state{port=P}=S) ->
    {stop, Reason, S#state{port=undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%% == private: state ==

-spec cleanup(state()) -> ok.
%% cleanup(#state{port=P,command=C}=S)
%%   when is_port(P), C >= ?CMD_ALL_ALLOC ->
%%     _ = ergen_driver:control(P, ?CMD_ALL_FREE, []),
%%     cleanup(S#state{command = 0});
%% cleanup(#state{port=P,command=C}=S)
%%   when is_port(P), C < ?CMD_ALL_ALLOC ->
%%     _ = ergen_driver:close(P),
%%     cleanup(S#state{port = undefined});
cleanup(#state{}) ->
    %%io:format("~p [~p:cleanup]~n", [self(),?MODULE]),
    ergen_util:flush().

-spec setup([any()]) -> {ok,state()}|{stop,_}.
setup([Id]) ->
    %%io:format("~p [~p:setup] id=~w~n", [self(),?MODULE, Id]),
    process_flag(trap_exit, true),
    {ok, #state{id = Id}};
setup(_) ->
    {stop, badarg}.

-spec setup(tuple(),[any()],state()) -> {ok,state()}|{error,_,state()}.
setup(Driver, Options, #state{port=undefined}=S)
  when is_tuple(Driver) ->
    case ergen_driver:open(Driver) of
        {ok, Port} ->
            setup(Driver, Options, S#state{port = Port});
        {error, Reason} ->
            {error, Reason, S}
    end;
setup(Driver, Options, #state{id=I,command=0}=S)
  when is_list(Options) ->
    L = [
         %% {?CMD_ALL_INIT,   [{id,I}|proplists:delete(id,Options)]},
         %% {?CMD_ALL_ALLOC,  []},
         %% {?CMD_ALL_CONFIG, proplists:delete(id,Options)}
        ],
    try lists:foldl(fun control/2, S, L) of
        State ->
            setup(Driver, Options, State)
    catch
        {Reason, Command} ->
            {error, Reason, S#state{command = Command}}
    end;
setup(_Driver, _Options, #state{}=S) ->
    {ok, S}.

%% == private: etc ==

-spec control({integer(),[any()]},state()) -> state().
control({Command,Args}, #state{port=P,command=C}=S)
  when Command > C ->
    case ergen_driver:control(P, Command, Args) of
        ok ->
            S#state{command = Command};
        {error, Reason} ->
            throw({Reason,C})
    end.
