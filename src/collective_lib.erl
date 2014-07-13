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

%% File    : collective_lib.erl
%% Author  : Alexander Koeck
%% Purpose : Collection of helper functions.

-module(collective_lib).

%% API
-export([ensure_started/1,
         json_safe_types/1,
         mfa_to_binary/1,
         to_atom/1,
         fill_template/2]).

%% ============================================================================
%% API functions
%% ============================================================================

%% Ensure an application is started; already_started is no error 
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        Error -> 
            Error
    end.

%% Convert things to json/jiffy safe types
-spec json_safe_types(any()) -> binary() | number() | atom() | null | [any()].
json_safe_types([]) ->
    [];
json_safe_types(undefined) ->
    null;
json_safe_types(Binary) when is_binary(Binary) ->
    Binary; 
json_safe_types(Number) when is_number(Number) ->
    Number; 
json_safe_types(Atom) when is_atom(Atom) ->
    Atom; 
json_safe_types(Pid) when is_pid(Pid) -> 
    list_to_binary(pid_to_list(Pid));
json_safe_types(Port) when is_port(Port) -> 
    list_to_binary(erlang:port_to_list(Port));
json_safe_types([H|T]) when is_integer(H) -> 
    % Dirty but good enough atm. Is there a better solution without 
    % pseudo typing?
    list_to_binary([H|T]);
json_safe_types([H|T]) ->
    [json_safe_types(H) | json_safe_types(T)];
json_safe_types({process, Pid}) when is_pid(Pid) -> 
    json_safe_types(Pid);
json_safe_types({process, {Name, Node}}) when is_atom(Name) and is_atom(Node) -> 
    json_safe_types(Name).

%% Convert things to atoms
-spec to_atom(atom() | binary()) -> atom().
to_atom(Atom) when is_atom(Atom) -> 
    Atom;
to_atom(Binary) when is_binary(Binary) -> 
    binary_to_atom(Binary, utf8).

%% Convert MFA tuples to binary strings
-spec mfa_to_binary({atom(), atom(), integer()}) -> <<_:16,_:_*8>>.
mfa_to_binary({M, F, A}) ->
    Module = atom_to_binary(M, utf8), 
    Function = atom_to_binary(F, utf8), 
    Arity = integer_to_binary(A),
    <<Module/binary, ":", Function/binary, "/", Arity/binary>>.

%% Fill given struct template with data 
-spec fill_template([{_,[any()] | {_,_}}],_) -> 
    [{_, null | binary() | [any()] | number()}].
fill_template([{Name, {key, Key}} | Tail], Data) when is_atom(Key) ->
    [{Name, json_safe_types(proplists:get_value(Key, Data))} 
        | fill_template(Tail, Data)];
fill_template([{Name, {value, Value}} | Tail], Data) ->
    [{Name, json_safe_types(Value)} | fill_template(Tail, Data)];
fill_template([{Name, {function, F}} | Tail], Data) when is_function(F) ->
    [{Name, F(Data)} | fill_template(Tail, Data)];
fill_template([{Name, List} | Tail], Data) when is_list(List) ->
    [{Name, fill_template(List, Data)} | fill_template(Tail, Data)];
fill_template([], _) -> [].





