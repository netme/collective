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

%% File    : collective_node_monitor.erl
%% Author  : Alexander Koeck
%% Purpose : Monitor nodes and collect status info

-module(collective_node_monitor).
-behaviour(gen_server).

% API
-export([start_link/0,
         update/0]).

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

-spec update() -> ok.
update() ->
	gen_server:cast(?MODULE, update).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    ok = net_kernel:monitor_nodes(true, [{node_type, all}]),
	{ok, #state{}}.

-spec handle_call(_, _, #state{}) -> {reply, ignore, #state{}}.
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

-spec handle_cast(_, #state{}) -> {noreply, #state{}}.
handle_cast(update, State) ->
    send_node_info(),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

-spec handle_info(_, #state{}) -> {noreply, #state{}}.
handle_info({nodeup, Node, _InfoList}, State) ->
    collective_node_master:assimilate(Node),
	send_node_info(Node),
    {noreply, State};
handle_info({nodedown, Node, _InfoList}, State) ->
	subspace:publish(collective, {delete, node, Node}),
    {noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(_, #state{}) -> ok.
terminate(_Reason, _State) ->
    ok = net_kernel:monitor_nodes(false).

-spec code_change(_, #state{}, _) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ============================================================================
%% internal functions
%% ============================================================================

%% Collect system info from all connected nodes
-spec send_node_info() -> ok.
send_node_info() ->
    [send_node_info(N) || N <- [node() | nodes(connected)]],
    ok.

%% Collect system info from given node and publish via subspace
-spec send_node_info(atom()) -> ok.
send_node_info(Node) ->
     Data = rpc:call(Node, erlang, apply, [
        fun() -> 
            Keys = [ 
                otp_release,
                smp_support,
                logical_processors,
                schedulers,
                system_architecture,
                thread_pool_size
            ],
            [{apps, apps_on_node()} | [{K, erlang:system_info(K)} || K <- Keys]]
        end, []]),
    case Data of 
	    {badrpc, _} ->
            error_logger:error_report(Data),
            ok = subspace:publish(collective, 
                {error, "RPC call to node ~p failed", [Node]});
        _ ->
            ok = subspace:publish(collective, {node, Node, Data})
    end.

%% ===================================================================
%% Appmon functions
%% ===================================================================

%% List local applications and application masters
-spec apps_on_node() -> [{application, atom(), pid()}].
apps_on_node() ->
    app_info(application:which_applications()).

%% Get master for given application {App, Desc, Vsn}
-spec app_info([{atom(), list(), list()}]) 
    -> [{application, atom(), pid()}].
app_info([{App, _Desc, _Vsn} | T]) ->
    case catch application_controller:get_master(App) of
        Pid when is_pid(Pid) -> [{application, App, Pid} | app_info(T)];
        _ -> app_info(T)
    end;
app_info([]) -> [].


