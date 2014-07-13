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

%% File    : collective_process_probe.erl
%% Author  : Alexander Koeck
%% Purpose : Collect process information.

-module(collective_process_probe).
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

-export([proc_info/1]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 2000).

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
    _ = erlang:send_after(?INTERVAL, self(), update),
	{ok, #state{}}.

-spec handle_call(_, _, #state{}) -> {reply, ignore, #state{}}.
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

-spec handle_cast(_, #state{}) -> {noreply, #state{}}.
handle_cast(update, State) ->
    send_proc_info(),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

-spec handle_info(_, #state{}) -> {noreply, #state{}}.
handle_info(update, State) ->
    send_proc_info(),
    _Ref = erlang:send_after(?INTERVAL, self(), update),
	{noreply, State};
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

%% Send process information via subspace
-spec send_proc_info() -> ok.
send_proc_info() ->
    Info = [proc_info(Pid)|| Pid <- processes(), is_process_alive(Pid)],
	subspace:publish(collective, {processes, node(), Info}).

%% Collect and prepare process information
-spec proc_info(pid()) -> {pid(), [{_,_}]}.
proc_info(Pid) -> 
    Keys = [ 
        %backtrace,
        %binary, 
        catchlevel, 
        current_function, 
        error_handler, 
        garbage_collection, 
        group_leader, 
        heap_size, 
        initial_call,
        last_calls, 
        links, 
        memory, 
        message_queue_len, 
        monitored_by,
        monitors, 
        priority, 
        reductions, 
        registered_name, 
        sequential_trace_token, 
        stack_size, 
        status, 
        suspending,
        total_heap_size, 
        trace, 
        trap_exit
    ], 
    Info = process_info(Pid, Keys),
    Info2 = flatten_monitors(Info),
    {Pid, Info2}.

%% Translate monitors to a flat list of pids
-spec flatten_monitors([{_,_}]) -> [{_,_}].
flatten_monitors(Info) -> 
    Monitors = [ 
        case M of 
            {process, {Name, _}} -> whereis(Name);
            {process, Pid} -> Pid
        end || M <- proplists:get_value(monitors, Info)],
    lists:keyreplace(monitors, 1, Info, {monitors, Monitors}).



