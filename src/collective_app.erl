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

%% File    : collective_app.erl 
%% Author  : Alexander Koeck
%% Purpose : Application callback module.

-module(collective_app).
-behaviour(application).

%% Application callbacks
-export([start/2, 
         stop/1]).

%% ============================================================================
%% Application callbacks
%% ============================================================================

-spec start(_, _) -> {ok, pid()}.
start(_Type, _Args) ->
    init(),
    connect(),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, collective, "index.html"}},
			{"/stream", collective_websocket, []},
			{"/[...]", cowboy_static, {priv_dir, collective, ""}}
		]}
	]),
    {ok, IP} = application:get_env(http_ip),
    {ok, Port} = application:get_env(http_port),
    {ok, Acceptors} = application:get_env(http_max_acceptors),
	{ok, _} = cowboy:start_http(http, Acceptors, [{ip, IP}, {port, Port}], [
		{env, [{dispatch, Dispatch}]}
	]),
	collective_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
	ok.

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec init() -> ok.
init() ->
    ok = application:ensure_started(subspace),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(ranch),
    ok = application:ensure_started(cowlib),
    ok = application:ensure_started(cowboy).

-spec connect() -> ok.
connect() ->
    {ok, Nodes} = application:get_env(collective, nodes),
    connect_nodes(Nodes).

-spec connect_nodes([atom()]) -> ok.
connect_nodes([H|T]) ->
    net_kernel:connect_node(H),
    connect_nodes(T);
connect_nodes([]) ->
    ok.


