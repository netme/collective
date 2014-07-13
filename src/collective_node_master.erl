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

%% File    : collective_node_master.erl
%% Author  : Alexander Koeck
%% Purpose : Add nodes to collective and inject probes

-module(collective_node_master).
-behaviour(gen_server).

% API
-export([start_link/0,
         assimilate/1]).

% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% ============================================================================
%% API
%% ============================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Inject collective modules and start probes
-spec assimilate(node()) -> ok.
assimilate(Node) ->
    gen_server:call(?MODULE, {assimilate, Node}).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    assimilate_nodes(nodes(connected)),
	{ok, #state{}}.

-spec handle_call(_, _, #state{}) -> {reply, ignore, #state{}}.
handle_call({assimilate, Node}, _From, State) ->
    assimilate_node(Node),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

-spec handle_cast(_, #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
	{noreply, State}.

-spec handle_info(_, #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(_, #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(_, #state{}, _) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ============================================================================
%% internal functions
%% ============================================================================

%% Inject modules and spawn Collective processes
%% TODO: handle running processes 
%% TODO: supervision tree
%% TODO: configuration for probes to inject
-spec assimilate_nodes([node()]) -> ok.
assimilate_nodes([]) ->
    ok;
assimilate_nodes([H|T]) ->
    assimilate_node(H),
    assimilate_nodes(T).

-spec assimilate_node(node()) -> ok.
assimilate_node(Node) ->
    inject_app(Node, collective),
    inject_app(Node, subspace),
    run_app(Node, subspace),
    
    inject_mod(Node, collective_process_probe),
    spawn(Node, collective_process_probe, start_link, []),
    ok.

%% Load module and inject into remote node
-spec inject_mod(atom(), atom()) -> ok.
inject_mod(Node, Mod) -> 
    {Mod, Bin, File} = code:get_object_code(Mod),
    {module, Mod} = rpc:call(Node, code, load_binary, [Mod, File, Bin]),
    ok.

%% Load and inject all modules of a given application into remote node
-spec inject_app(atom(), atom()) -> ok.
inject_app(Node, App) -> 
    {ok, Modules} = application:get_key(App, modules),
    _ = [ inject_mod(Node, Mod) || Mod <-Modules],
    ok.

%% Start application on remote node
-spec run_app(atom(), atom()) -> ok.
run_app(Node, App) -> 
    {ok, Desc} = application:get_all_key(subspace),
    _ = rpc:call(Node, application, load, [{application, App, Desc}]),
    ok = rpc:call(Node, collective_lib, ensure_started, [App]).

