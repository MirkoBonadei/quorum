-module(quorum).
-behaviour(gen_statem).

-export([start/2, stop/1]).
-export([init/1, handle_event/4, code_change/4, terminate/3]).

%% RAFT FSM states
-export([follower/3, candidate/3, leader/3]).

%% API functions
start(NodeName, Options) ->
    gen_statem:start({local, NodeName}, 
                     ?MODULE,
                     Options,
                     []).

stop(ProcessName) ->
    gen_statem:stop(ProcessName).

follower({call, From}, _EventContent, _Data) ->
    {keep_state_and_data, [{reply, From, sending_vote}]}.

candidate(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

leader(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

%% Behaviour callbacks

init(#{cluster_config := []} = Options) ->
    {stop, cluster_configuration_error};
init(#{cluster_config := ClusterConfig} = Options) when (erlang:length(ClusterConfig)) rem 2 == 1 ->
    {stop, cluster_configuration_error};
init(#{cluster_config := ClusterConfig} = Options) ->
    DefaultState = #{cluster_config => ClusterConfig,
                     voted_for => none,
                     current_term => 0,
                     log => [],
                     storage_path => volatile},
    State = case maps:find(storage_path, Options) of
                error ->
                    DefaultState;
                {ok, StoragePath} ->
                    maps:update(storage_path, StoragePath, DefaultState)
                    % Load previous state if present or save current
            end,
    {state_functions, follower, State}.

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {state_functions, OldState, OldData}.

terminate(_Reason, _State, _Data) ->
    ignored.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.
