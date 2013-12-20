%% =============================================================================
%% Copyright 2013 AONO Tomohiko
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

-module(eroonga_port).

-include("eroonga_internal.hrl").

%% -- public --
-export([start_link/1, stop/1]).
-export([call/3, cast/3]).

%% -- behaviour: gen_server --
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- private --
-record(state, {
          port :: port(),
          path :: binary(),
          assigned :: dict()
         }).

%% == public ==

-spec start_link([property()]) -> {ok,pid()}|ignore|{error,_}.
start_link(Args)
  when is_list(Args) ->
    case gen_server:start_link(?MODULE, [], []) of
        {ok, Pid} ->
            case gen_server:call(Pid, {setup,Args}, infinity) of
                ok ->
                    {ok, Pid};
                {error, Reason} ->
                    stop(Pid),
                    {error, Reason}
            end;
        Other ->
            Other
    end.

-spec stop(pid()) -> ok.
stop(Pid)
  when is_pid(Pid) ->
    gen_server:call(Pid, stop).

-spec call(pid(),integer(),[term()]) -> term().
call(Pid, Command, Args)
  when is_pid(Pid), is_integer(Command), is_list(Args) ->
    case cast(Pid, Command, Args) of
        ok ->
            receive {_Ref, Reply} -> Reply end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec cast(pid(),integer(),[term()]) -> term().
cast(Pid, Command, Args)
  when is_pid(Pid), is_integer(Command), is_list(Args) ->
    gen_server:call(Pid, {command,Command,Args}).

%% == behaviour: gen_server ==

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({command,Command,Args}, From, #state{port=P,assigned=A}=S)
  when is_integer(Command), is_list(Args)->
    case eroonga_driver:command(P, R=make_ref(), Command, Args) of
        ok ->
            {reply, ok, S#state{assigned = dict:append(R,From,A)}};
        {error, Reason} ->
            {reply, {error,Reason}, S}
    end;
handle_call({setup,Args}, _From, #state{}=S) ->
    try lists:foldl(fun setup/2, S, Args) of
        State ->
            {reply, ok, State}
    catch
        {Reason, State} ->
            {reply, {error,Reason}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error,badarg}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({P,{data,Data}}, #state{port=P,assigned=A}=S)
  when is_binary(Data) ->
    {R, Reply} = binary_to_term(Data),
    case dict:find(R, A) of
        {ok, [Client]} ->
            _ = gen_server:reply(Client, Reply),
            {noreply, S#state{assigned = dict:erase(R,A)}};
        error ->
            {stop, {error,enoent}, S}
    end;
handle_info({'EXIT',_Pid,Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% == private: state ==

cleanup(#state{port=P}=S)
  when undefined =/= P ->
    _ = eroonga_driver:close(P),
    cleanup(S#state{port = undefined});
cleanup(#state{}) ->
    eroonga_util:flush().

setup([]) ->
    process_flag(trap_exit, true),
    {ok, #state{assigned = dict:new()}}.

setup({driver,Term}, #state{port=undefined}=S)
  when is_tuple(Term) ->
    case eroonga_driver:open(Term) of
        {ok, Port} ->
            S#state{port = Port};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({path,Term}, #state{port=P}=S)
  when is_port(P) ->
    Path = if is_binary(Term) -> Term;
              is_list(Term)   -> list_to_binary(Term);
              true -> throw({badarg,path})
           end,
    case eroonga_driver:call(P, ?ERN_CONTROL_DB_OPEN, [Path]) of % or control
        ok ->
            S#state{path = Path};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup(_Ignore, #state{}=S) ->
    S.
