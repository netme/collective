%% Copyright (c) 2014, Alexander Koeck
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without 
%% modification, are permitted provided that the following conditions are met:
%%
%% 1. Redistributions of source code must retain the above copyright notice, 
%% this list of conditions and the following disclaimer.
%%
%% 2. Redistributions in binary form must reproduce the above copyright notice, 
%% this list of conditions and the following disclaimer in the documentation 
%% and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
%% POSSIBILITY OF SUCH DAMAGE.

%% File    : collective_websocket.erl
%% Author  : Alexander Koeck
%% Purpose : WebSocket data stream interface.

-module(collective_websocket).
-behaviour(cowboy_websocket_handler).

-include_lib("subspace/include/subspace_records.hrl").

%% Websocket callbacks
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-record(state, {}).

%% ============================================================================
%% Websocket callbacks
%% ============================================================================

-spec init({tcp, http}, _, _) -> {upgrade, protocol, cowboy_websocket}.
init({tcp, http}, _Req, _Opts) ->
	subspace:subscribe(collective),
	{upgrade, protocol, cowboy_websocket}.

-spec websocket_init(atom(), cowboy_req:req(), _) -> {ok, cowboy_req:req(), _}.
websocket_init(_Type, Req, _Opts) ->
	collective_node_monitor:update(),
	{ok, Req, #state{}}.

-spec websocket_handle({'text', binary()}, cowboy_req:req(), _) -> 
    {ok, cowboy_req:req(), _} | {reply, {text,binary()}, cowboy_req:req(), _}.
websocket_handle({text, JSON}, Req, State) ->
    Data = jiffy:decode(JSON),
    case handle(Data) of 
        ok -> 
            {ok, Req, State};
        Reply ->
            {reply, Reply, Req, State}
    end.

-spec websocket_info(#subspace{}, cowboy_req:req(), _) -> 
    {ok, cowboy_req:req(), _} | {reply, {text, binary()}, cowboy_req:req(), _}.
websocket_info(#subspace{topic=collective, message=Data}, Req, State) -> 
    JSON = prepare(Data),
	{reply, JSON, Req, State};
websocket_info(Info, Req, State) ->
    io:format("Unknown message: ~p~n", [Info]),
	{noreply, Req, State}.

-spec websocket_terminate(_, _, _) -> ok.
websocket_terminate(_Reason, _Req, _State) ->
	ok.

%% ============================================================================
%% Internal functions
%% ============================================================================

%% Prepare data structure for JSON reply 
-spec prepare({atom(), _, _}) -> {'text', binary()}.
prepare({processes, Node, ProcessInfoList}) -> 
    Data = [ process_map(Node, Info) || Info <- ProcessInfoList],
    reply(processes, Node, Data);
prepare({node, Node, NodeInfo}) -> 
    Template = [
        {id, {value, Node}},
        {release, {key, otp_release}},
        {smp, {key, smp_support}},
        {cpus, {key, logical_processors}},
        {schedulers, {key, schedulers}},
        {architecture, {key, system_architecture}},
        {threads, {key, thread_pool_size}},
        {apps, {function, get_apps_fun()}}
    ],
    Data = collective_lib:fill_template(Template, NodeInfo),
    reply(node, Node, {Data});
prepare({delete, Type, ID}) -> 
    reply(delete, Type, ID);
prepare({error, Format, Args}) -> 
    reply(error, Format, Args);
prepare({Type, ID, Data}) -> 
    reply(Type, ID, Data).

%% Encode JSON and return WebSocket reply tuple
-spec reply(atom(), [1..255]|atom(), any()) -> {text, binary()}.
reply(error, Format, Args) ->  
    Str = io_lib:format(Format, Args),
    Bin = erlang:iolist_to_binary(Str),
    {text, jiffy:encode({[{type, error}, {message, Bin}]})};
reply(delete, Type, ID) ->  
    {text, jiffy:encode({[ {type, Type}, {id, ID}, {delete, true} ]}) };
reply(Type, ID, Data) ->  
    {text, jiffy:encode({[ {type, Type}, {id, ID}, {data, Data} ]}) }.

%% Handle JSON request frame
-spec handle(_) -> ok | {text, binary()}.
handle([<<"connect">>, NewNode, NewCookie]) -> 
    Node = collective_lib:to_atom(NewNode),
    Cookie = collective_lib:to_atom(NewCookie),
    erlang:set_cookie(Node, Cookie),
    case net_adm:ping(Node) of
        pong -> ok;
        _ ->  reply(error, "Connection to ~p failed", [Node])
    end;
handle(Data) ->
    error_logger:error_report(["no handler for frame type", {data, Data}]),
    ok. 

%% Map process structure to Jiffy-compatible format
-spec process_map(atom(), {pid(), [any()]}) -> {[{_,_},...]}.
process_map(Node, {Pid, Info}) ->
    Id = collective_lib:json_safe_types(Pid),
    Template = [
        {id, {value, Id}},
        {node, {value, Node}},
        {name, {key, registered_name}},
        {initial_fun, {function, get_mfa_fun(initial_call)}},
        {current_fun, {function, get_mfa_fun(current_function)}},
        {trap_exit, {key, trap_exit}},
        {msgq, {key, message_queue_len}},
        {reds, {key, reductions}},
        {memory, {key, heap_size}},
        {group_leader, {key, group_leader}},
        {links, {key, links}},
        {monitors, {key, monitors}}
    ],
    Data = collective_lib:fill_template(Template, Info),
    %error_logger:info_report({node_data, Data}),
    {Data}.

%% Return mapping fun for application tuples
-spec get_apps_fun() -> fun(([any()]) -> [{_}]).
get_apps_fun() -> 
    fun(Data) ->
        Apps = proplists:get_value(apps, Data),
        [{[
            {name, Name}, 
            {master, collective_lib:json_safe_types(Pid)}
        ]} || {application, Name, Pid} <- Apps]
    end.

%% Return mapping fun for mfa tuples
-spec get_mfa_fun(atom()) -> fun(([any()]) -> <<_:16,_:_*8>>).
get_mfa_fun(Key) -> 
    fun(Data) ->
        MFA = proplists:get_value(Key, Data),
        collective_lib:mfa_to_binary(MFA)
    end.

